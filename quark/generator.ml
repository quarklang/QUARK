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
  | A.Transpose -> ".adjoint()"
  | other -> A.str_of_unop other

let gen_postop = A.str_of_postop

let gen_basictype = function
  | T.Int -> "int"
  | T.Float -> "float"
  | T.Bool -> "bool"
  | T.Fraction -> "Frac"
  | T.Complex -> "std::complex<float>"
  | T.Qreg -> "Qureg"
  | T.String -> "std::string"
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

(* system temp var (20 char long)*)
(* must take a unit arg to make this a function *)
let gen_temp_var _ = 
  let rec seq = function
    | 0 -> []
    | x -> 0 :: seq (x - 1)
  in
  List.fold_left ( fun acc _ ->
      (* randomly generate from 3 classes: number, lower/upper case letters *)
      let rand_char =
        match Random.int 3 with
        | 0 -> Char.chr (48 + Random.int 10)
        | 1 -> Char.chr (65 + Random.int 26)
        | 2 -> Char.chr (97 + Random.int 26)
        | _ -> '_'
      in
      acc ^ Char.escaped rand_char
  ) Builtin.forbidden_prefix (seq 10)


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
  | S.StringLit(s) -> 
    gen_basictype T.String ^ "(\"" ^ s ^ "\")"

  (* compound literals *)
  | S.FractionLit(num_ex, denom_ex) -> 
    two_arg "Frac" (gen_expr num_ex) (gen_expr denom_ex) 

  | S.QRegLit(qex1, qex2) -> 
    two_arg "Qureg::create<true>" (gen_expr qex1) (gen_expr qex2)

  | S.ComplexLit(real_ex, im_ex) -> 
    two_arg (gen_basictype T.Complex) (gen_expr real_ex) (gen_expr im_ex)

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
      (* blablamat.adjoint() *)
    | S.OpMatrixTranspose -> surr @@ (surr ex_code) ^ (gen_unop op)
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
    (* handle print specially *)
    if Builtin.is_print func_id then
      if List.length ex_list = 0 then ""
      else
        let delim = " << " in
        let cout_code = List.fold_left (
          fun acc ex -> acc ^delim^ gen_expr ex
        ) 
        (* C++ prints 'true/false' instead of '1/0' *)
        "std::cout << std::boolalpha << std::setprecision(6)" ex_list 
        in
        cout_code ^ if func_id = "print" then " << std::endl" else ""

    else if func_id = "apply_oracle" then
      let code_list = ex_to_code_list ex_list in
      (* de-string arg #2, which is actually a function parameter *)
      let arg2 = List.nth code_list 1 in
      let str_type_len = String.length (gen_basictype T.String) in
      let arg2 = String.sub arg2 
          (str_type_len+2) ((String.length arg2) - (str_type_len+4)) in
      let code_list = [List.nth code_list 0; arg2; List.nth code_list 2] in
      more_arg func_id code_list
      
    else (* non-special cases *)
      more_arg func_id (ex_to_code_list ex_list)
  
  (* Membership testing with keyword 'in' *)
  | S.Membership(elem, array) -> 
    (* from quarklang.h *)
    two_arg "membership_in" (gen_expr elem) (gen_expr array)
    
  | _ -> fail_unhandle "expr"

(* helper: expr_list -> code(string)_list *)
and ex_to_code_list ex_list =
  List.map (fun ex -> gen_expr ex) ex_list


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
        let code_while = handle_compound stmt in
        "while (" ^ gen_expr pred_ex ^")" ^
        code_while ^ "// end while"
        
      | S.ForStatement(iter, stmt) -> 
        let code_for = handle_compound stmt in
        begin
        match iter with
        | S.ArrayIterator(typ, id, array_ex) -> 
          let array_code = gen_expr array_ex in
          "for (" ^ gen_datatype typ ^" "^ id ^" : "^ array_code ^")"
            ^ code_for ^ " // end for-array" 
        | S.RangeIterator(typ, id, range) -> (
          match range with
          | S.Range(iter_type, start_ex, end_ex, step_ex) -> 
            (* pre-store the results in system-reserved temp variables *)
            let start_code = gen_expr start_ex in
            let end_code = gen_expr end_ex in
            let step_code = gen_expr step_ex in

            let end_temp = gen_temp_var () in
            let step_temp = gen_temp_var () in
            (* determines the direction of iteration *)
            let dir_temp = gen_temp_var () in

            let init = if typ = A.NoneType then id 
                     else gen_datatype typ ^" "^ id in
            let temp_type = gen_datatype iter_type in
              (* store step_expr to a temp var *)
              temp_type ^" "^ end_temp ^" = "^ end_code ^";\n"
              (* store step_expr to a temp var *)
            ^ temp_type ^" "^ step_temp ^" = "^ step_code ^";\n"
            ^ temp_type ^" "^ dir_temp^ " = "^ step_temp ^ " > 0 ? 1 : -1;\n"
            ^ "for (" ^ init ^ " = " ^ start_code ^ "; "
            ^ dir_temp ^" * "^ id ^ " < " ^ dir_temp ^" * "^ end_code ^ "; "
            ^ id ^ " += " ^ step_temp ^ ")" 
            ^ code_for ^ " // end for-range" 

          | _ -> fail_unhandle "RangeIterator"
          )
        end

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