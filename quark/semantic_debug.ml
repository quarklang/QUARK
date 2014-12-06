module A = Ast
module S = Sast
module T = Type
module Gen = Generator

module StrMap = Map.Make(String)

type func_info = {
  f_args: S.decl list;
  f_return: A.datatype;
}

(* 
let func_example = { 
  f_ident = A.Ident "shit"; 
  f_args = [S.PrimitiveDecl(A.DataType T.Int, A.Ident "gugu")]; 
  f_return = A.DataType T.Float };; *)

(* map string ident name to datatype or function info *)
type environment = {
    var_table: A.datatype StrMap.t;
    func_table: func_info StrMap.t;
}

(* surround with parenthesis *)
let surr str = "(" ^ str ^ ")"

let get_id (A.Ident name) = name

(* DEBUG ONLY *)
(* print out the environment *)
let debug_s_decl_list f_args =
  let paramStr = 
    List.fold_left 
      (fun s param -> s ^ ((function 
        | S.PrimitiveDecl(typ, id) -> 
		    (Gen.gen_datatype typ) ^ " " ^ (get_id id)
        | _ -> "FATAL") param) ^ ", ") "" f_args
  in
  if paramStr = "" then ""
  else
    String.sub paramStr 0 ((String.length paramStr) - 2)
  
let debug_env env =
  begin
    print_string "ENV{var= ";
    StrMap.iter 
      (fun key v -> print_string @@ key ^ ": " ^ Gen.gen_datatype v ^ "; ")
      env.var_table;
    print_string "\nfunc= ";
    StrMap.iter 
      (fun key f_table -> print_string @@ 
        key ^ ": " ^ debug_s_decl_list f_table.f_args ^ " => " ^ Gen.gen_datatype f_table.f_return)
      env.func_table;
    print_endline "}";
  end

let gen_unop = function
  A.Neg -> "-"
| A.Not -> "!"
| A.BitNot -> "~"

let gen_postop = function
  A.Inc -> "++"
| A.Dec -> "--"

let gen_binop = function
  A.Add -> "+"
| A.Sub -> "-"
| A.Mul -> "*"
| A.Div -> "/"
| A.Mod -> "%"
| A.Pow -> "**"
| A.Lshift -> "<<"
| A.Rshift -> ">>"
| A.Less -> "<"
| A.LessEq -> "<="
| A.Greater -> ">"
| A.GreaterEq -> ">="
| A.Eq -> "=="
| A.NotEq -> "!="
| A.BitAnd -> "&"
| A.BitXor -> "^"
| A.BitOr -> "|"
| A.And -> "&&"
| A.Or -> "||"
| A.AddEq -> "+="
| A.SubEq -> "-="
| A.MulEq -> "*="
| A.DivEq -> "/="
| A.AndEq -> "&="
| _ -> failwith "unhandled binop"

let gen_vartype = function
  | T.Int -> "int64_t"
  | T.Float -> "float"
  | T.Bool -> "bool"
  | T.Fraction -> "Frac"
  | T.Complex -> "complex<float>"
  | T.QReg -> "Qureg"
  | T.String -> "string"
  | T.Void -> "void"

let rec gen_datatype = function
	| A.DataType(t) -> 
    gen_vartype t
	| A.ArrayType(t) -> 
		gen_datatype t ^ "[]"
	| A.MatrixType(t) -> 
   (match t with
    | A.DataType(matType) -> 
      (match matType with
      (* only support 3 numerical types *)
      | T.Int | T.Float | T.Complex -> 
      "Matrix<" ^ gen_vartype matType ^ ", Dynamic, Dynamic>"
      | _ -> failwith "Non-numerical matrix type")
    (* we shouldn't support float[][[]] *)
    | _ -> 
      failwith "Bad matrix type")


let rec gen_expr = function
  (* literals *)
  | A.IntLit(x) | A.FloatLit(x) | A.BoolLit(x) -> 
    x
  | A.StringLit(s) -> 
    "\"" ^ s ^ "\""

  | A.ArrayLit(exlist) -> 
    "[" ^ gen_expr_list exlist ^ "]"

  | A.MatrixLit(exlistlist) -> 
    "Matrix<>(" ^ gen_matrix_list exlistlist ^ ")"

  | A.FractionLit(exNum, exDenom) -> 
    "Frac(" ^ gen_expr exNum ^ ", " ^ gen_expr exDenom ^ ")"

  | A.QRegLit(ex1, ex2) -> 
    "Qureg::create<true>(" ^ gen_expr ex1 ^ ", " ^ gen_expr ex2 ^ ")"

  | A.ComplexLit(exReal, exImag) -> 
    "complex<float>(" ^ gen_expr exReal ^ ", " ^ gen_expr exImag ^ ")"
  
  (* Binary ops *)
  (* '+' used for matrix addition, '&' for array concatenation *)
  | A.Binop(ex1, op, ex2) -> 
    let ex1 = gen_expr ex1 in
    let ex2 = gen_expr ex2 in
     (match op with
      | A.Query -> "measure_top(" ^ex1^ ", " ^ex2^ ", true)"
      | A.QueryUnreal -> "measure_top(" ^ex1^ ", " ^ex2^ ", false)"
      | _ -> ex1 ^" "^ gen_binop op ^" "^ ex2)
  
  (* Unary ops *)
  | A.Unop(op, ex) -> 
    gen_unop op ^ gen_expr ex
  
  (* Assignment *)
  | A.Assign(lval, ex) -> 
    gen_lvalue lval ^ " = " ^ gen_expr ex
  | A.Lval(lval) -> 
    gen_lvalue lval
  
  (* Special assignment *)
  | A.AssignOp(lval, op, ex) -> 
    gen_lvalue lval ^" "^ gen_binop op ^" "^ gen_expr ex
  | A.PostOp(lval, op) -> 
    gen_lvalue lval ^" "^ gen_postop op
    
  (* Membership testing with keyword 'in' *)
  | A.Membership(exElem, exArray) -> 
    (* !!!! Needs to assign exElem and exArray to compiled temp vars *)
    (* Shouldn't change over calls!!! *)
    let exElem = gen_expr exElem in
    let exArray = gen_expr exArray in
      "std::find(" ^surr exArray^ ".begin(), " ^surr exArray^ ".end(), " ^
      exElem^ ") != " ^surr exArray^ ".end()"
    
  (* Function calls *)
  | A.FunctionCall(funcId, exlist) -> 
    get_id funcId ^ surr( gen_expr_list exlist )
  
  | _ -> failwith "some expr not parsed"

and gen_expr_list exlist =
  let exlistStr = 
    List.fold_left 
      (fun s ex -> s ^ gen_expr ex ^ ", ") "" exlist
  in
  (* get rid of the last 2 chars ', ' *)
  if exlistStr = "" then ""
  else
    String.sub exlistStr 0 ((String.length exlistStr) - 2)
    
and gen_lvalue = function
  | A.Variable(id) -> 
    get_id id
  | A.ArrayElem(id, exlist) -> 
    get_id id ^ "[" ^ gen_expr_list exlist ^ "]"

and gen_matrix_list exlistlist =
  let exlistlistStr = 
    List.fold_left 
      (fun s exlist -> s ^ gen_expr_list exlist ^ "; ") "" exlistlist
  in
  (* get rid of the last 2 chars ', ' *)
  if exlistlistStr = "" then ""
  else
    String.sub exlistlistStr 0 ((String.length exlistlistStr) - 2)
  

let gen_s_param = function 
  | A.PrimitiveDecl(typ, id) -> 
    S.PrimitiveDecl(typ, id)
  | _ -> failwith "Function parameter list declaration error"

let gen_s_param_list param_list =
  List.map 
    (fun param -> gen_s_param param) param_list
  
	
let rec gen_range id = function
	| A.Range(exStart, exEnd, exStep) -> 
    let exStart = gen_expr exStart in
    let exEnd = gen_expr exEnd in
    let exStep = gen_expr exStep in
    (* !!!! Needs to assign exStart, exEnd and exStep to compiled temp vars *)
    (* Shouldn't change over iteration!!! *)
    let exCmp = surr exEnd ^">"^ surr exStart ^" ? " in
    let id = get_id id in
      "(" ^id^ "=" ^exStart^ "; " ^
      exCmp^ id^" < " ^surr exEnd^ " : " ^id^ " > " ^surr exEnd^ "; " ^
      exCmp^ id^" += " ^surr exStep^ " : " ^id^ " -= " ^surr exStep^ ")"
  | _ -> failwith "range fatal error"
	
let rec gen_iterator = function
  | A.RangeIterator(id, rng) -> 
    gen_range id rng
  | A.ArrayIterator(id, ex) -> 
    get_id id ^ " in " ^ gen_expr ex
	
let rec gen_s_decl = function
  | A.AssigningDecl(typ, id, ex) -> 
    S.AssigningDecl(typ, id, S.BoolLit("DUMMY", A.DataType(T.Bool))) (* TODO gen_s_expr *)
  | A.PrimitiveDecl(typ, id) -> 
    S.PrimitiveDecl(typ, id)


(* Main entry point: take stmts (AST) and convert to SAST *)
let rec gen_sast env stmts =
  match stmts with
  | [] -> []
  | stmt :: rest ->
    let s_stmt =
      match stmt with
			(* top level statements *)
      | A.FunctionDecl(return_type, func_id, param_list, stmt_list) ->
        let _ = debug_env env in
        let s_param_list = gen_s_param_list param_list in
        let func_entry = { 
          f_args = s_param_list; 
          f_return = return_type
        } in
        let func_table' = StrMap.add (get_id func_id) func_entry env.func_table in
        let env' = { 
          var_table = env.var_table; 
          func_table = func_table'
        } in
        let _ = debug_env env' in
        S.FunctionDecl(return_type, func_id, s_param_list, 
          gen_sast env' stmt_list)
        
        (*
        let funcId = get_id funcId in
          begin
            print_endline @@ gen_datatype returnTyp ^ " " ^ 
              funcId ^ surr( gen_param_list paramList );
            print_endline @@ "{ // start " ^ funcId;
            gen_sast stmtList;
            print_endline @@ "} // end " ^ funcId ^ "\n";
          end *)
      
      | A.ForwardDecl(returnTyp, funcId, paramList) -> 
        S.EmptyStatement
          (* print_endline @@ "*forward* " ^ gen_datatype returnTyp ^ " " ^ 
            (get_id funcId) ^ surr( gen_param_list paramList ) ^";\n"; *)

      (* statements *)
      | A.IfStatement(ex, stmtIf, stmtElse) -> 
        S.EmptyStatement
        (* begin
          print_endline @@ "if " ^ surr(gen_expr ex);
          print_endline "{ // start if";
          gen_sast [stmtIf];
          print_endline "else";
          gen_sast [stmtElse];
          print_endline "} // end if";
        end *)
				
      | A.WhileStatement(ex, stmt) -> 
        S.EmptyStatement
        (* begin
          print_endline @@ "while " ^ surr(gen_expr ex);
          print_endline "{ // start while";
          gen_sast [stmt];
          print_endline "} // end while";
        end *)
            
      | A.ForStatement(iter, stmt) -> 
        S.EmptyStatement
        (* begin
          (* for (a in 1:5, b in 7:3:-1) *)
          (* List.iter (fun iter -> 
            print_endline @@ "for " ^ gen_iterator iter) iterList; *)
          print_endline @@ "for " ^ gen_iterator iter;
          print_endline "{ // start for";
          gen_sast [stmt];
          print_endline "} // end for";
        end *)
            
      | A.CompoundStatement(stmtList) -> 
        S.EmptyStatement
        (* begin
          print_endline "{ // start compound";
          gen_sast stmtList;
          print_endline "} // end compound";
        end *)

      | A.Declaration(dec) -> 
        S.Declaration(gen_s_decl dec)
      | A.Expression(ex) -> 
        S.EmptyStatement
        (* print_endline @@ gen_expr ex ^ ";" *)
      | A.ReturnStatement(ex) -> 
        S.EmptyStatement
        (* print_endline @@ "return " ^ gen_expr ex ^ ";" *)
      | A.EmptyStatement -> 
        S.EmptyStatement
        (* print_endline ";" *)
      | A.VoidReturnStatement -> 
        S.EmptyStatement
        (* print_endline "return; // void" *)
      | A.BreakStatement -> 
        S.EmptyStatement
        (* print_endline "break; // control" *)
      | A.ContinueStatement -> 
        S.EmptyStatement
        (* print_endline "continue; // control" *)
      | _ -> failwith "nothing for eval()"
    in 
    s_stmt :: gen_sast env rest
