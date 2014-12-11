module A = Ast
module S = Sast
module T = Type

let header_code =
 "#include \"qureg.h\"\n" ^
 "#include \"qumat.h\"\n" ^
 "#include \"qugate.h\"\n" ^
 "#include \"quarklang.h\"\n\n" ^
 "using namespace Qumat;\n" ^
 "using namespace Qugate;\n\n"

(* surround with parenthesis *)
let surr str = "(" ^ str ^ ")"

(* subtle differences from Ast print *)
let gen_binop = function
  | A.Mod -> "%"
  | A.Pow -> "pow"
  | A.And -> "&&"
  | A.Or -> "||"
  | other -> A.str_of_binop other

let gen_unop = function
  | A.Not -> "!"
  | other -> A.str_of_unop other

let gen_postop = A.str_of_postop

let gen_basictype = function
  | T.Int -> "int"
  | T.Float -> "float"
  | T.Bool -> "bool"
  | T.Fraction -> "Frac"
  | T.Complex -> "complex<float>"
  | T.Qreg -> "Qureg"
  | T.String -> "string"
  | T.Void -> "void"

let rec gen_datatype = function
	| A.DataType(t) -> 
    gen_basictype t
	| A.ArrayType(t) -> 
		"vector<" ^ gen_datatype t ^ ">"
	| A.MatrixType(t) -> (
    match t with
    | A.DataType(matType) -> (
      match matType with
      (* only support 3 numerical types *)
      | T.Int | T.Float | T.Complex -> 
      "Matrix<" ^ gen_basictype matType ^ ", Dynamic, Dynamic>"
      | _ -> failwith 
        "INTERNAL codegen non-numerical matrix in gen_datatype"
      )
    (* we shouldn't support float[][[]] *)
    | _ -> 
      failwith "INTERNAL bad matrix type to str"
    )
  | A.NoneType -> failwith "INTERNAL codegen NoneType in gen_datatype"

(******* Utilities *******)
(* handles "2, 3, 4, " -> "2, 3, 4" *)
let trim_last str =
  if String.length str > 1 then
    String.sub str 0 ((String.length str) - 2)
  else str

(* generate Func(arg1, arg2) code *)
let two_arg func code1 code2 =
  func ^"("^ code1 ^", "^ code2 ^")"


(* generate Func(arg1, arg2) code *)
let more_arg_helper left_delimiter right_delimiter func code_list =
  let codes = 
    List.fold_left (
    fun acc code -> acc ^ code ^ ", "
    ) "" code_list in
  let codes = trim_last codes in
  func ^left_delimiter^ codes ^right_delimiter
  
let more_arg = more_arg_helper "(" ")"

let array_arg = more_arg_helper "{ " " }"
  
(* common error msg: unhandled case that shouldn't happen *)
let fail_unhandle msg = 
  failwith @@ "INTERNAL codegen unhandled " ^ msg


(********* Main expr -> code string entry *********)
let rec gen_expr = function
  (* simple literals *)
  | S.IntLit(i) -> i
  | S.BoolLit(b) -> b
  | S.FloatLit(f) -> f
  | S.StringLit(s) -> "string(\"" ^ s ^ "\")"

  (* compound literals *)
  | S.FractionLit(num_ex, denom_ex) -> 
    two_arg "Frac" (gen_expr num_ex) (gen_expr denom_ex) 

  | S.QRegLit(qex1, qex2) -> 
    two_arg "Qureg::create<true>" (gen_expr qex1) (gen_expr qex2)

  | S.ComplexLit(real_ex, im_ex) -> 
    two_arg "complex<float>" (gen_expr real_ex) (gen_expr im_ex)

  | S.ArrayLit(arr_type, ex_list) ->
    array_arg (gen_datatype arr_type) (ex_to_code_list ex_list)

  | S.ArrayCtor(arr_type, dim) ->
    gen_datatype arr_type ^"("^ gen_expr dim ^")"

  | S.MatrixLit(elem_type, ex_list, coldim) ->
    (* we flatten the matrix to be a vector *)
    let flattened = gen_expr 
        (S.ArrayLit( A.ArrayType(elem_type), ex_list))
    in
    (* a utility function from quarklang.h *)
    two_arg "matrix_literal" (string_of_int coldim) flattened
  
  | S.MatrixCtor(mat_type, rowdim, coldim) ->
    two_arg (gen_datatype mat_type ^ "::Zero")
        (gen_expr rowdim) (gen_expr coldim)

  (* Binary ops *)
  (* '+' used for matrix addition, '&' for array concatenation *)
  | S.Binop(expr1, op, expr2, optag) ->
    let expr1_code = gen_expr expr1 in
    let expr2_code = gen_expr expr2 in
    (* cast helpers *)
    let cast_complex ex_code = 
      two_arg "complex<float>" ex_code "0.0" in
    let cast_fraction ex_code = 
      two_arg "Frac" ex_code "1" in
    let parenthize code1 op code2 =
      surr @@ code1 ^" "^ gen_binop op ^" "^ code2
    in begin
    match optag with
    | S.OpVerbatim -> 
      if op = A.Pow then (* special: not infix! *)
        two_arg "pow" expr1_code expr2_code
      else
        parenthize expr1_code op expr2_code
    | S.CastComplex1 -> 
      parenthize (cast_complex expr1_code) op expr2_code
    | S.CastComplex2 -> 
      parenthize expr1_code op (cast_complex expr2_code)      
    | S.CastFraction1 -> 
      parenthize (cast_fraction expr1_code) op expr2_code
    | S.CastFraction2 -> 
      parenthize expr1_code op (cast_fraction expr2_code)
    | S.OpArrayConcat ->  
      two_arg "concat_vector" expr1_code expr2_code
    | S.OpStringConcat -> 
      parenthize expr1_code A.Add expr2_code
    | S.OpMatrixKronecker -> 
      two_arg "kronecker_mat" expr1_code expr2_code
    | _ -> fail_unhandle "optag in binop"
    end
    
  (* Query ops *)
  | S.Queryop(qreg_ex, op, start_ex, end_ex, optag) -> 
    let qreg_code = gen_expr qreg_ex in
    let start_code = gen_expr start_ex in
    let end_code = gen_expr end_ex in
    let real_flag = if op = A.Query then "true" else "false" in
    begin
    match optag with
    | S.OpQuerySingleBit -> 
      more_arg "measure" [qreg_code; start_code; real_flag]
    | S.OpVerbatim -> 
      more_arg "measure_range" [qreg_code; start_code; end_code; real_flag]
    | _ -> fail_unhandle "optag in queryop"  
    end
    
  (* Unary ops *)
  | S.Unop(op, ex, optag) -> 
    let ex_code = gen_expr ex in begin
    match optag with
    | S.OpVerbatim -> surr @@ (gen_unop op) ^ ex_code
    | _ -> fail_unhandle "optag in unop"
    end
  
  | S.Lval(lval) -> begin
    match lval with
    | S.Variable(id) -> id
    | S.ArrayElem(id, ex_list) -> 
      let subscripts =
        List.fold_left (
          fun acc ex -> 
            acc ^"["^ gen_expr ex ^"]"
        ) "" ex_list
      in
      id ^ subscripts
    | S.MatrixElem(id, ex_list) -> 
      (* hackish: Eigen lib access matrix elem just like funcall *)
      more_arg id (ex_to_code_list ex_list)
    end
    
  (* Post ++ and -- *)
  | S.PostOp(lval, op) -> 
    surr @@ gen_expr (S.Lval(lval)) ^" "^ gen_postop op
      
  (* Assignment *)
  | S.Assign(lval, rhs_ex) -> 
    surr @@ gen_expr (S.Lval(lval)) ^" = "^ gen_expr rhs_ex

  (* Function calls *)
  | S.FunctionCall(func_id, ex_list) -> 
    more_arg func_id (ex_to_code_list ex_list)
  
  (* Membership testing with keyword 'in' *)
  | S.Membership(elem, array) -> 
    failwith "Membership not yet supported"
    (* !!!! Needs to assign exElem and exArray to compiled temp vars *)
    (*
    let exElem = gen_expr exElem in
    let exArray = gen_expr exArray in
      "std::find(" ^surr exArray^ ".begin(), " ^surr exArray^ ".end(), " ^
      exElem^ ") != " ^surr exArray^ ".end()"
    *)
    
  | _ -> fail_unhandle "expr"

(* helper: expr_list -> code(string)_list *)
and ex_to_code_list ex_list =
  List.map (fun ex -> gen_expr ex) ex_list

(*

(* for-loop iterator syntax *)  
let gen_s_range env id = function
	| A.Range(start_ex, end_ex, step_ex) -> 
    let vtype = (get_env_var env id).v_type in
    let idstr = get_id id in
    match vtype with
    | A.NoneType -> failwith @@ "For-iterator " ^idstr^ " undefined"
    | A.DataType(T.Int) | A.DataType(T.Float) -> begin
      let s_start_ex, start_type = gen_expr start_ex in
      let s_end_ex, end_type = gen_expr end_ex in
      let s_step_ex, step_type = gen_expr step_ex in
      match start_type, end_type, step_type with
      | A.DataType(typ1), A.DataType(typ2), A.DataType(typ3) -> 
        if not (typ1 = T.Float || typ1 = T.Int) ||
           not (typ2 = T.Float || typ2 = T.Int) ||
           not (typ3 = T.Float || typ3 = T.Int) then
           failwith @@ "Unsupported range type: " ^ T.str_of_type typ1 ^ ", "
               ^ T.str_of_type typ2 ^ ", " ^ T.str_of_type typ3
        else
           S.Range(s_start_ex, s_end_ex, s_step_ex)
      | _ -> failwith @@ "Unsupported range type: " ^ A.str_of_datatype start_type ^ ", "
               ^ A.str_of_datatype end_type ^ ", " ^ A.str_of_datatype step_type
      end
    | _ -> failwith @@ "Unsupported for-iterator " ^idstr^ ": " ^A.str_of_datatype vtype

let gen_s_iter env = function
  | A.RangeIterator(typ, id, range) -> (
    (* if typ = NoneType, there's no new iterator variable defined in the loop *)
    match typ with
    | A.NoneType ->
      S.RangeIterator(typ, get_id id, gen_s_range env id range)
    | _ -> 
      (* add the declared var to scope *)
      let env', _ = gen_s_decl env (A.PrimitiveDecl(typ, id)) in
      env', S.RangeIterator(typ, get_id id, gen_s_range env' id range)
    )

  | A.ArrayIterator(typ, id, array_ex) -> 
    let idstr = get_id id in
    let env', s_array_ex, array_type = gen_expr array_ex in
    let env', _ = gen_s_decl env (A.PrimitiveDecl(typ, id)) in
    let elem_type = match array_type with 
      | A.ArrayType(elem_type) -> elem_type
      | _ -> failwith @@ 
        "Array-style for-loop must have array type, not " ^ A.str_of_datatype array_type
    in
    (* check iterator variable and list consistency *)
    let _ = match typ, elem_type with
      | A.DataType(T.Int), A.DataType(T.Float)
      | A.DataType(T.Float), A.DataType(T.Int) -> ()
      | typ', elem_type' when typ' = elem_type' -> ()
      | _ -> failwith @@ "For-loop has incompatible types: " ^ A.str_of_datatype typ 
          ^" "^idstr^ " but " ^ A.str_of_datatype elem_type ^ " expected"
    in
    env', S.ArrayIterator(typ, idstr, s_array_ex)
    
    
(* When if/while/for are followed by a non-compound single-line stmt, *)
(* we need to go one scope deeper *)
let handle_compound_env env = function
  | A.CompoundStatement(_) -> env
  | _ -> incr_env_depth env

*)


(* Used in A.FunctionDecl *)
let gen_param_list param_list =
  List.fold_left 
    (fun accstr param -> accstr ^
      ((function 
      | S.PrimitiveDecl(typ, id) -> gen_datatype typ ^ " " ^ id
      | _ -> fail_unhandle "gen_param_list"
      ) param) ^ ", "
    ) "" param_list
    
    
(********** Main entry point: SAST -> string **********)
let rec gen_code = function
  | [] -> ""
  | stmt :: rest ->
    let stmt_code =
      match stmt with
			(* top level statements *)
      | S.FunctionDecl(return_type, func_id, param_list, stmt_list) ->
        let param_list_code = gen_param_list param_list in
        let stmt_list_code = gen_code stmt_list in
        gen_datatype return_type ^ " " ^ func_id ^ "(" 
        ^ trim_last param_list_code ^ ")\n"
        ^ "{\n" ^ stmt_list_code 
        ^ "\n} // end " ^ func_id ^ "()\n"
      
      | S.ForwardDecl(return_type, func_id, param_list) -> 
        let param_list_code = gen_param_list param_list in
        gen_datatype return_type ^ " " ^ func_id ^ "(" 
        ^ trim_last param_list_code ^ ");\n"

      (* statements *)
      | S.IfStatement(pred_ex, stmt_if, stmt_else) -> 
        let code_if = handle_compound stmt_if in
        let code_else = handle_compound stmt_else in
        "if (" ^ gen_expr pred_ex ^")" ^
        code_if ^ "\nelse " ^ code_else ^ "// end if"
				
      | S.WhileStatement(pred_ex, stmt) -> 
        ""
        
      | S.ForStatement(iter, stmt) -> 
        ""
            
      | S.CompoundStatement(stmt_list) -> 
        let stmt_list_code = gen_code stmt_list in
        "{\n" ^ stmt_list_code ^ "\n}"

      | S.Declaration(dec) ->
        let code =
          match dec with
          | S.AssigningDecl(typ, id, ex) -> 
            gen_datatype typ ^" "^ id ^" = "^ gen_expr ex
          | S.PrimitiveDecl(typ, id) -> 
            gen_datatype typ ^" "^ id
        in code ^ "; "

      | S.Expression(ex) -> (gen_expr ex) ^ ";"

      | S.ReturnStatement(ex) -> "return " ^ gen_expr ex ^ ";" 

      | S.VoidReturnStatement -> "return;"

      | S.BreakStatement -> "break;"

      | S.ContinueStatement -> "continue;"

      | S.EmptyStatement -> ";"

      | _ -> failwith "INTERNAL codegen unhandled statement"
    in 
    let new_line = if List.length rest = 0 then "" else "\n" in
    stmt_code ^ new_line ^ gen_code rest

and 
(* if/for/while make a stmt a compound if it isn't *)
handle_compound stmt =
  match stmt with
  | S.CompoundStatement(_) -> gen_code [stmt]
  | _ -> gen_code [S.CompoundStatement([stmt])]