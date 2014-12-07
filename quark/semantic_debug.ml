module A = Ast
module S = Sast
module T = Type
module Gen = Generator

module StrMap = Map.Make(String)

(* utilities *)
let fst_2 = function x, _ -> x;;
let snd_2 = function _, x -> x;;
let fst_3 = function x, _, _ -> x;;
let snd_3 = function _, x, _ -> x;;
let trd_3 = function _, _, x -> x;;

let get_id (A.Ident name) = name

(****** Environment definition ******)
type func_info = {
  f_args: A.datatype list;
  f_return: A.datatype;
  f_defined: bool; (* for forward declaration *)
}

type var_info = {
  v_type: A.datatype;
  v_depth: int; (* how deep in scope *)
}

(* map string ident name to datatype or function info *)
type environment = {
    var_table: var_info StrMap.t;
    func_table: func_info StrMap.t;
    (* current function name waiting for 'return' *)
    (* if "", we are not inside any function *)
    func_current: string; 
    depth: int;
}

(************** DEBUG ONLY **************)
(* print out the func decl param list *)
let debug_s_decl_list f_args =
  let paramStr = 
    List.fold_left 
      (fun s typ -> s ^ (Gen.gen_datatype typ) ^ ", ") "" f_args
  in
  if paramStr = "" then ""
  else
    String.sub paramStr 0 ((String.length paramStr) - 2)
  
(* print out the func decl param list *)
let debug_env env msg =
  begin
    print_endline @@ "ENV " ^ msg ^ "{";
    print_string "Var= ";
    StrMap.iter 
      (fun key vinfo -> print_string @@ 
      key ^ ": " ^ Gen.gen_datatype vinfo.v_type ^ "(" ^ string_of_int vinfo.v_depth ^ "); ")
      env.var_table;
    print_string "\nFunc= ";
    StrMap.iter 
      (fun key finfo -> print_endline @@ 
        key ^ "(" ^ string_of_bool finfo.f_defined ^ "): " ^ 
        debug_s_decl_list finfo.f_args ^ " => " ^ Gen.gen_datatype finfo.f_return ^ ";")
      env.func_table;
    print_endline @@ "Current= " ^ env.func_current;
    print_endline "}";
  end

(****** Environment var_table ******)
(* return updated var_table field (keep env.depth) *)
let update_var_table env var_typ var_id =
  StrMap.add (get_id var_id)
    {
      v_type = var_typ;
      v_depth = env.depth
    }
    env.var_table
  
(* if doesn't exist, return NoneType *)
let get_env_var env var_id =
  try
    StrMap.find (get_id var_id) env.var_table
  with Not_found -> { 
    v_type = A.NoneType; 
    v_depth = -1
  }

let update_env_var env var_typ var_id =
  let vinfo = get_env_var env var_id in
  match vinfo.v_type with
  | A.NoneType 
  | _ when vinfo.v_depth < env.depth ->  (* we can safely add the var if it's in the inner scope *)
  {
    var_table = update_var_table env var_typ var_id;
    func_table = env.func_table;
    func_current = env.func_current;
    depth = env.depth;
  }
  | _ -> failwith @@ "Variable redeclaration: " ^ Gen.gen_datatype var_typ ^ " " ^ get_id var_id

(* go one scope deeper *)
let incr_env_depth env = 
  {
    var_table = env.var_table;
    func_table = env.func_table;
    func_current = env.func_current;
    depth = env.depth + 1;
  }

(****** Environment func_table ******)
let get_env_func env func_id =
  try
    StrMap.find (get_id func_id) env.func_table
  with Not_found -> { 
    f_args = []; 
    f_return = A.NoneType;
    f_defined = false;
  }
  
(* Used in A.FunctionDecl *)
(* add all formal params to updated var_table *)
let update_env_func env return_type func_id s_param_list is_defined =
  let finfo = get_env_func env func_id in
  let errmsg_str = ": " ^ get_id func_id ^ "()" in
  let s_arg_types = 
    List.map (function
        | S.PrimitiveDecl(typ, id) -> typ
        | _ -> failwith @@ "Function parameter list declaration error" ^ errmsg_str
        ) s_param_list in
  (* add formal params to var scope. This is a lambda function *)
  let add_formal_var_lambda = 
    List.fold_left 
        (fun env -> function
        | S.PrimitiveDecl(typ, id) -> 
          update_env_var env typ id
        | _ -> failwith @@ "Function parameter list declaration error" ^ errmsg_str) in
  (* utility function *)
  let add_new_func_table_to_env func_table' =
    { 
      var_table = env.var_table;
      func_table = StrMap.add (get_id func_id) func_table' env.func_table;
      func_current = 
        (* if forward_decl, we don't have a func_current *)
        if is_defined then get_id func_id else ""; 
      depth = env.depth;
    } in
   
  match finfo.f_return with
  | A.NoneType -> begin
    let func_table' = { 
      (* only keep the formal param types *)
      f_args = s_arg_types; 
      f_return = return_type;
      f_defined = is_defined
    } in
    let env' = add_new_func_table_to_env func_table' in
    if is_defined then
      (* add the formal param idents to scope *)
      add_formal_var_lambda env' s_param_list
    else
      (* simply forward decl, don't add stuff to scope *)
      env'
    end
  | _ when not finfo.f_defined ->
    if is_defined then
      (* check param list and return type, should be the same *)
      if finfo.f_return = return_type && finfo.f_args = s_arg_types then
        let func_table' = { 
          f_args = finfo.f_args; 
          f_return = finfo.f_return;
          f_defined = true
        } in
        let env' = add_new_func_table_to_env func_table' in
        add_formal_var_lambda env' s_param_list
      else
        failwith @@ "Incompatible forward declaration" ^ errmsg_str
    else
      failwith @@ "Function forward redeclaration" ^ errmsg_str
  | _ -> failwith @@ "Function redefinition" ^ errmsg_str



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

(* return env', S.expr, type *)
let rec gen_s_expr env = function
  (* simple literals *)
  | A.IntLit(i) -> env, S.IntLit(i), T.Int
  | A.BoolLit(b) -> env, S.BoolLit(b), T.Bool
  | A.FloatLit(f) -> env, S.FloatLit(f), T.Float
  | A.StringLit(s) -> env, S.StringLit(s), T.String

  | A.FractionLit(num_expr, denom_expr) -> 
    env, S.IntLit("TODO"), T.Int
      (*
      let env, num_expr = gen_s_expr env num_expr in
      let env, denom_expr = gen_s_expr env denom_expr in
      env, S.FractionLit(num_expr, denom_expr), Ty.Fraction
      *)

  | A.QRegLit(expr1, expr2) -> 
    env, S.IntLit("TODO"), T.Int
      (* checks expressions first *)
      (*
      let env, expr1 = gen_s_expr env expr1 in
      let env, expr2 = gen_s_expr env expr2 in
      S.QRegLit(expr1, expr2, A.DataType(T.QReg)), env
      *)

  | A.ComplexLit(real_expr, im_expr) -> 
    env, S.IntLit("TODO"), T.Int
      (* checks expressions first *)
      (*
      let real_expr, env   = gen_s_expr real_expr env in
      let im_expr, env = gen_s_expr im_expr env in
      S.ComplexLit(real_expr, im_expr, A.DataType(T.Complex)), env
      *)

  | A.ArrayLit(exprs) ->
    env, S.IntLit("TODO"), T.Int
      (*
      let arr, typ, env = check_array exprs env in
      S.ArrayLit(arr, A.DataType(typ)), env
      *)

  | A.MatrixLit(exprs_list_list) ->
    env, S.IntLit("TODO"), T.Int
      (*
      (* each expression list (aka row) must be the same length.
       * each expression list must be a valid expression list. *)
      let matrix, matrix_type, _, env = List.fold_left

          (fun (rows, curr_type, row_length, env) exprs -> 
              (* evaluate each row where each row is an expr list *)
              let exprs, row_type, env = check_array exprs env in
              let exprs_length = List.length exprs in

              match curr_type with
                  | T.Void -> 
                      (* means this is the 1st row which means we now know the matrix type *)
                      (exprs :: rows), row_type, exprs_length, env

                  | matrix_type ->
                      (* ensure all rows have the same type ... *)
                      if matrix_type <> row_type
                      then raise(Error "All elements in a matrix must be the same type");

                      (* ... and are the same length*)
                      if row_length <> exprs_length
                      then raise(Error "All rows in a matrix must be the same length");

                      (exprs :: rows), row_type, row_length, env)

          ([], T.Void, 0, env) exprs_list_list in
      S.MatrixLit(List.rev matrix, A.DataType(matrix_type)), env
      *)
  
  (* Binary ops *)
  (* '+' used for matrix addition, '&' for array concatenation *)
  | A.Binop(ex1, op, ex2) -> 
    env, S.IntLit("TODO"), T.Int
      (*
    let ex1 = gen_s_expr ex1 in
    let ex2 = gen_s_expr ex2 in
     (match op with
      | A.Query -> "measure_top(" ^ex1^ ", " ^ex2^ ", true)"
      | A.QueryUnreal -> "measure_top(" ^ex1^ ", " ^ex2^ ", false)"
      | _ -> ex1 ^" "^ gen_binop op ^" "^ ex2)
      *)
  
  (* Unary ops *)
  | A.Unop(op, ex) -> 
    env, S.IntLit("TODO"), T.Int
      (*
    gen_unop op ^ gen_s_expr ex
      *)
  
  (* Assignment *)
  | A.Assign(lval, ex) -> 
    env, S.IntLit("TODO"), T.Int
      (*
    gen_lvalue lval ^ " = " ^ gen_s_expr ex
      *)
  | A.Lval(lval) -> 
    env, S.IntLit("TODO"), T.Int
      (*
    gen_lvalue lval
      *)
  
  (* Special assignment *)
  | A.AssignOp(lval, op, ex) -> 
    env, S.IntLit("TODO"), T.Int
      (*
    gen_lvalue lval ^" "^ gen_binop op ^" "^ gen_s_expr ex
      *)
  | A.PostOp(lval, op) -> 
    env, S.IntLit("TODO"), T.Int
      (*
    gen_lvalue lval ^" "^ gen_postop op
      *)
    
  (* Membership testing with keyword 'in' *)
  | A.Membership(exElem, exArray) -> 
    env, S.IntLit("TODO"), T.Int
      (*
    (* !!!! Needs to assign exElem and exArray to compiled temp vars *)
    (* Shouldn't change over calls!!! *)
    let exElem = gen_s_expr exElem in
    let exArray = gen_s_expr exArray in
      "std::find(" ^surr exArray^ ".begin(), " ^surr exArray^ ".end(), " ^
      exElem^ ") != " ^surr exArray^ ".end()"
      *)
    
  (* Function calls *)
  | A.FunctionCall(funcId, exlist) -> 
    env, S.IntLit("TODO"), T.Int
      (*
    get_id funcId ^ surr( gen_expr_list exlist )
      *)
  
  | _ -> failwith "some expr not parsed"

(*
and gen_expr_list exlist =
  let exlistStr = 
    List.fold_left 
      (fun s ex -> s ^ gen_s_expr ex ^ ", ") "" exlist
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
  

	
let rec gen_range id = function
	| A.Range(exStart, exEnd, exStep) -> 
    let exStart = gen_s_expr exStart in
    let exEnd = gen_s_expr exEnd in
    let exStep = gen_s_expr exStep in
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
    get_id id ^ " in " ^ gen_s_expr ex
*)

let gen_s_param = function 
  | A.PrimitiveDecl(typ, id) -> 
    S.PrimitiveDecl(typ, id)
  | _ -> failwith "Function parameter list declaration error"

(* Used in A.FunctionDecl *)
let gen_s_param_list param_list =
  List.map 
    (fun param -> gen_s_param param) param_list
    
(* decl *)
let rec gen_s_decl env = function
  (* update_env_var checks redeclaration error *)
  | A.AssigningDecl(typ, id, ex) -> 
    let env' = update_env_var env typ id in
    (env', S.AssigningDecl(typ, id, S.BoolLit("TODO"))) (* TODO gen_s_expr *)
  | A.PrimitiveDecl(typ, id) -> 
    let env' = update_env_var env typ id in
    (env', S.PrimitiveDecl(typ, id))


(* Main entry point: take AST and convert to SAST *)
(* return env, [stmt] *)
let rec gen_sast env = function
  | [] -> (env, [])
  | stmt :: rest ->
    let env_new, s_stmt =
      match stmt with
			(* top level statements *)
      | A.FunctionDecl(return_type, func_id, param_list, stmt_list) ->
        let _ = debug_env env "before FunctionDecl" in
        let s_param_list = gen_s_param_list param_list in
        let env' = incr_env_depth env in
        let env' = update_env_func env' return_type func_id s_param_list true in
        let _ = debug_env env' "after FunctionDecl" in
        (* get the function declaration, then close 'func_current' *)
        let function_decl = S.FunctionDecl(return_type, func_id, s_param_list, 
          snd_2 @@ gen_sast env' stmt_list) in
        let env'' = { 
          var_table = env.var_table; 
          func_table = env'.func_table;
          func_current = "";
          depth = env.depth;
        } in
        let _ = debug_env env'' "closed after FuncDecl" in
        (env'', function_decl)
          
        (*
        let funcId = get_id funcId in
          begin
            print_endline @@ gen_datatype returnTyp ^ " " ^ 
              funcId ^ surr( gen_param_list paramList );
            print_endline @@ "{ // start " ^ funcId;
            gen_sast stmtList;
            print_endline @@ "} // end " ^ funcId ^ "\n";
          end *)
      
      | A.ForwardDecl(return_type, func_id, param_list) -> 
        let s_param_list = gen_s_param_list param_list in
        let env' = update_env_func env return_type func_id s_param_list false in
        (env', S.ForwardDecl(return_type, func_id, s_param_list))
          (* print_endline @@ "*forward* " ^ gen_datatype returnTyp ^ " " ^ 
            (get_id funcId) ^ surr( gen_param_list paramList ) ^";\n"; *)

      (* statements *)
      | A.IfStatement(ex, stmtIf, stmtElse) -> 
        (env, S.EmptyStatement)
        (* begin
          print_endline @@ "if " ^ surr(gen_s_expr ex);
          print_endline "{ // start if";
          gen_sast [stmtIf];
          print_endline "else";
          gen_sast [stmtElse];
          print_endline "} // end if";
        end *)
				
      | A.WhileStatement(ex, stmt) -> 
        (env, S.EmptyStatement)
        (* begin
          print_endline @@ "while " ^ surr(gen_s_expr ex);
          print_endline "{ // start while";
          gen_sast [stmt];
          print_endline "} // end while";
        end *)
            
      | A.ForStatement(iter, stmt) -> 
        (env, S.EmptyStatement)
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
        (env, S.EmptyStatement)
        (* begin
          print_endline "{ // start compound";
          gen_sast stmtList;
          print_endline "} // end compound";
        end *)

      | A.Declaration(dec) -> 
        let env', s_dec = gen_s_decl env dec in
        let _ = debug_env env' "after decl" in
        (env', S.Declaration(s_dec))

      | A.Expression(ex) -> 
        let env', s_ex, _ = gen_s_expr env ex in
        (env', S.Expression(s_ex))
        (* print_endline @@ gen_s_expr ex ^ ";" *)

      | A.ReturnStatement(ex) -> 
        (env, S.EmptyStatement)
        (* print_endline @@ "return " ^ gen_s_expr ex ^ ";" *)

      | A.EmptyStatement -> 
        (env, S.EmptyStatement)

      | A.VoidReturnStatement -> 
        (env, S.EmptyStatement)
        (* print_endline "return; // void" *)

      | A.BreakStatement -> 
        (env, S.EmptyStatement)
        (* print_endline "break; // control" *)

      | A.ContinueStatement -> 
        (env, S.EmptyStatement)
        (* print_endline "continue; // control" *)

      | _ -> failwith "nothing for eval()"
    in 
    let env_new, s_rest = gen_sast env_new rest in
    (env_new, (s_stmt :: s_rest))
