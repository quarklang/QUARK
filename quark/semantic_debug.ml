module A = Ast
module S = Sast
module T = Type

(* surround with parenthesis *)
let surr str = "(" ^ str ^ ")"

let gen_id (A.Ident name) = name

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
    gen_id funcId ^ surr( gen_expr_list exlist )
  
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
    gen_id id
  | A.ArrayElem(id, exlist) -> 
    gen_id id ^ "[" ^ gen_expr_list exlist ^ "]"

and gen_matrix_list exlistlist =
  let exlistlistStr = 
    List.fold_left 
      (fun s exlist -> s ^ gen_expr_list exlist ^ "; ") "" exlistlist
  in
  (* get rid of the last 2 chars ', ' *)
  if exlistlistStr = "" then ""
  else
    String.sub exlistlistStr 0 ((String.length exlistlistStr) - 2)
  

let rec gen_param = function 
  | A.PrimitiveDecl(typ, id) -> 
		(gen_datatype typ) ^ " " ^ (gen_id id)
  | _ -> failwith "decl list fatal error"

let rec gen_param_list paramList =
  let paramStr = 
    List.fold_left 
      (fun s param -> s ^ gen_param param ^ ", ") "" paramList
  in
  if paramStr = "" then ""
  else
    String.sub paramStr 0 ((String.length paramStr) - 2)
	
let rec gen_range id = function
	| A.Range(exStart, exEnd, exStep) -> 
    let exStart = gen_expr exStart in
    let exEnd = gen_expr exEnd in
    let exStep = gen_expr exStep in
    (* !!!! Needs to assign exStart, exEnd and exStep to compiled temp vars *)
    (* Shouldn't change over iteration!!! *)
    let exCmp = surr exEnd ^">"^ surr exStart ^" ? " in
    let id = gen_id id in
      "(" ^id^ "=" ^exStart^ "; " ^
      exCmp^ id^" < " ^surr exEnd^ " : " ^id^ " > " ^surr exEnd^ "; " ^
      exCmp^ id^" += " ^surr exStep^ " : " ^id^ " -= " ^surr exStep^ ")"
  | _ -> failwith "range fatal error"
	
let rec gen_iterator = function
  | A.RangeIterator(id, rng) -> 
    gen_range id rng
  | A.ArrayIterator(id, ex) -> 
    gen_id id ^ " in " ^ gen_expr ex
	
let rec gen_decl = function
  | A.AssigningDecl(typ, id, ex) -> 
    gen_datatype typ ^ " " ^ gen_id id ^ " = " ^ gen_expr ex
  | A.PrimitiveDecl(typ, id) -> 
    gen_datatype typ ^ " " ^ gen_id id


let rec gen_sast stmts =
  match stmts with
  | [] -> ()
  | stmt :: rest ->
    begin
      match stmt with
			(* top level statements *)
      | A.FunctionDecl(returnTyp, funcId, paramList, stmtList) ->
        let funcId = gen_id funcId in
          begin
            print_endline @@ gen_datatype returnTyp ^ " " ^ 
              funcId ^ surr( gen_param_list paramList );
            print_endline @@ "{ // start " ^ funcId;
            gen_sast stmtList;
            print_endline @@ "} // end " ^ funcId ^ "\n";
          end
      
      (* TODO: get rid of forward decl *)
      | A.ForwardDecl(returnTyp, funcId, paramList) -> 
          print_endline @@ "*forward* " ^ gen_datatype returnTyp ^ " " ^ 
            (gen_id funcId) ^ surr( gen_param_list paramList ) ^";\n";

      (* statements *)
      | A.IfStatement(ex, stmtIf, stmtElse) -> 
        begin
          print_endline @@ "if " ^ surr(gen_expr ex);
          print_endline "{ // start if";
          gen_sast [stmtIf];
          print_endline "else";
          gen_sast [stmtElse];
          print_endline "} // end if";
        end
				
      | A.WhileStatement(ex, stmt) -> 
        begin
          print_endline @@ "while " ^ surr(gen_expr ex);
          print_endline "{ // start while";
          gen_sast [stmt];
          print_endline "} // end while";
        end
            
      | A.ForStatement(iter, stmt) -> 
        begin
          (* for (a in 1:5, b in 7:3:-1) *)
          (* List.iter (fun iter -> 
            print_endline @@ "for " ^ gen_iterator iter) iterList; *)
          print_endline @@ "for " ^ gen_iterator iter;
          print_endline "{ // start for";
          gen_sast [stmt];
          print_endline "} // end for";
        end
            
      | A.CompoundStatement(stmtList) -> 
        begin
          print_endline "{ // start compound";
          gen_sast stmtList;
          print_endline "} // end compound";
        end

      | A.Declaration(dec) -> 
        print_endline @@ gen_decl dec ^ ";"
      | A.Expression(ex) -> 
        print_endline @@ gen_expr ex ^ ";"
      | A.ReturnStatement(ex) -> 
        print_endline @@ "return " ^ gen_expr ex ^ ";"
      | A.EmptyStatement -> 
        print_endline ";"
      | A.VoidReturnStatement -> 
        print_endline "return; // void"
      | A.BreakStatement -> 
        print_endline "break; // control"
      | A.ContinueStatement -> 
        print_endline "continue; // control"
      | _ -> failwith "nothing for eval()"
    end;
    gen_sast rest
