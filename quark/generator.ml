module A = Ast
module S = Sast
module T = Type

let header_code =
 "#include \"qureg.h\"\n" ^
 "#include \"qumat.h\"\n" ^
 "#include \"qugate.h\"\n" ^
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
        "INTERNAL non-numerical matrix in codegen gen_datatype"
      )
    (* we shouldn't support float[][[]] *)
    | _ -> 
      failwith "INTERNAL bad matrix type to str"
    )
  | A.NoneType -> failwith "INTERNAL NoneType in codegen gen_datatype"

(******* Utilities *******)
(* handles "2, 3, 4, " -> "2, 3, 4" *)
let trim_last str =
  if String.length str > 1 then
    String.sub str 0 ((String.length str) - 2)
  else str

(* generate Func(arg1, arg2) code *)
let two_arg func code1 code2 =
  func ^"("^ code1 ^", "^ code2 ^")"


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

  | S.ArrayLit(elem_type, ex_list) ->
    "ARRAY TODO"

  | S.MatrixLit(elem_type, ex_list, coldim) ->
    "MATRIX TODO"
  
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
    let put_space code1 op code2 =
      code1 ^" "^ gen_binop op ^" "^ code2
    in begin
    match optag with
    | S.OpVerbatim -> 
      if op = A.Pow then (* special: not infix! *)
        two_arg "pow" expr1_code expr2_code
      else
        put_space expr1_code op expr2_code
    | S.CastComplex1 -> 
      put_space (cast_complex expr1_code) op expr2_code
    | S.CastComplex2 -> 
      put_space expr1_code op (cast_complex expr2_code)      
    | S.CastFraction1 -> 
      put_space (cast_fraction expr1_code) op expr2_code
    | S.CastFraction2 -> 
      put_space expr1_code op (cast_fraction expr2_code)
    | S.OpArrayConcat ->  
      two_arg "concat_vector" expr1_code expr2_code
    | S.OpStringConcat -> 
      put_space expr1_code A.Add expr2_code
    | S.OpMatrixKronecker -> 
      two_arg "kronecker_mat" expr1_code expr2_code
    | _ -> failwith "INTERNAL unhandle optag in codegen binop"
    end
    
  (* Query ops *)
  | S.Queryop(qreg_ex, op, start_ex, end_ex, optag) -> 
    ""
    (*
    let s_qreg_ex, qreg_type = gen_expr qreg_ex in
    begin
      match qreg_type with 
      | A.DataType(T.Qreg) ->
        let s_start_ex, start_type = gen_expr start_ex in
        let s_end_ex, end_type = gen_expr end_ex in
        let optag = match s_end_ex with
          (* dummy literal from parser *)
        | S.IntLit("QuerySingleBit") -> S.OpQuerySingleBit
        | _ -> S.OpVerbatim in (
        match start_type, end_type with
        | A.DataType(T.Int), A.DataType(T.Int) -> 
          (* query check success *)
          S.Queryop(s_qreg_ex, op, s_start_ex, s_end_ex, optag), A.DataType(T.Int)
        | _ -> failwith @@ "Incompatible query args: " ^ A.str_of_datatype start_type 
          ^ (if optag = S.OpVerbatim then ", " ^ A.str_of_datatype end_type else "")
        )
      | _ -> failwith @@ 
          "Measurement must be queried on a qureg, not " ^ A.str_of_datatype qreg_type
    end
    *)
    
  (* Unary ops *)
  | S.Unop(op, ex, optag) -> 
    let ex_code = gen_expr ex in begin
    match optag with
    | S.OpVerbatim -> (gen_unop op) ^ ex_code
    | _ -> failwith "INTERNAL unhandle optag in codegen unop"
    end
  
  | S.Lval(lval) -> 
    "LVAL_TODO"
    (*
    let s_lval, ltype = match lval with
    | A.Variable(id) -> 
      let vtype = (get_env_var env id).v_type in
      let idstr = get_id id in
      if vtype = A.NoneType then
        failwith @@ "Variable " ^ idstr ^ " is undefined"
      else
        S.Variable(idstr), vtype
        
    (* Array/matrix lvalue e.g. arr[2,3,4] *)
    | A.ArrayElem(id, ex_list) -> 
      let vtype = (get_env_var env id).v_type in
      let idstr = get_id id in
      if vtype = A.NoneType then
        failwith @@ "Array/Matrix " ^ idstr ^ " is undefined"
      else
        let sub_dim = List.length ex_list in (* subscript [2,3,4] dimension *)
        let s_ex_list = (* check subscript types, must all be ints *)
          List.map (fun ex -> 
            let _, s_ex, typ = gen_expr ex in
            if typ = A.DataType(T.Int) then s_ex
            else failwith @@ "Subscript contains non-int: " 
                ^ idstr ^"["^ A.str_of_datatype typ ^ "]") ex_list
        in
        match vtype with
        (* Array lvalue *)
        | A.ArrayType(elem_type) -> 
          (* dim(original array) = dim(result lval) + dim(subscript) *)
          (* think of this as de-[] operation *)
          let rec get_array_lval_type sub_dim elem =
            if sub_dim = 0 then elem else
            match elem with
            | A.DataType(_) ->
              failwith @@ "Bad subscript dimension for array: " ^idstr
            | A.ArrayType(elem') ->
              get_array_lval_type (sub_dim - 1) elem'
              (* assume decl has already checked that matrix type is valid *)
            | A.MatrixType(A.DataType(raw_elem)) ->
              if sub_dim = 2 then A.DataType(raw_elem)
              else failwith @@ 
                  "Bad subscript dimension for array that contains matrix: " ^idstr
            | _ -> failwith @@ "INTERNAL bad array type: " ^ idstr
          in
          let lval_type = get_array_lval_type (sub_dim - 1) elem_type in
          let _ = print_endline @@ "DEBUG LVALUE "^idstr^" -> "^A.str_of_datatype lval_type in
          S.ArrayElem(idstr, s_ex_list), lval_type

        (* Matrix lvalue *)
        | A.MatrixType(elem_type) -> 
          if sub_dim = 2 then
            match elem_type with
            | A.DataType(_) -> 
              S.MatrixElem(idstr, s_ex_list), elem_type
            | _ -> failwith @@ 
                "INTERNAL bad matrix type should've been handled in S.decl: " ^idstr
          else
            failwith @@ "Subscript of matrix " ^idstr 
                ^ " must have 2 args, but " ^ string_of_int sub_dim ^ " provided"
                
        (* bad lvalue *)
        | _ -> failwith @@ idstr ^ " is not an array/matrix"
    in
    S.Lval(s_lval), ltype
    *)
    
  (* Post ++ and -- *)
  | S.PostOp(lval, op) -> 
    "POSTOP_TODO"
    (*
    let s_lval_ex, typ = gen_expr (A.Lval(lval)) in
      let s_lval = match s_lval_ex with
      | S.Lval(s_lval) -> s_lval
      | _ -> failwith "INTERNAL in postop: doesn't return S.Lval as expected"
      in (
      match typ with
      | A.DataType(T.Int)  | A.DataType(T.Float) -> 
        S.PostOp(s_lval, op), typ
      | _ -> failwith @@ "Incompatible operand for post op " 
          ^ A.str_of_postop op ^ ": " ^ A.str_of_datatype typ
      )
     *)
      
  (* Assignment *)
  | S.Assign(lval, rhs_ex) -> 
    "ASSIGN_TODO"
    (*
    let s_lval_ex, l_type = gen_expr (A.Lval(lval)) in
    let s_rhs_ex, r_type = gen_expr rhs_ex in
      let s_lval = match s_lval_ex with
      | S.Lval(s_lval) -> s_lval
      | _ -> failwith "INTERNAL in postop: doesn't return S.Lval as expected"
      in
      let return_type = if l_type = r_type then l_type
        else
        match l_type, r_type with
        | A.DataType(T.Int), A.DataType(T.Float) -> A.DataType(T.Int)
        | A.DataType(T.Float), A.DataType(T.Int) -> A.DataType(T.Float)
        | _ -> failwith @@ "Assignment type mismatch: "
            ^ A.str_of_datatype l_type ^" = "^ A.str_of_datatype r_type
      in
      let _ = print_endline @@ "DEBUG ASSIGN returns "^A.str_of_datatype return_type in
      S.Assign(s_lval, s_rhs_ex), return_type
     *)

  (* Function calls *)
  | S.FunctionCall(func_id, ex_list) -> 
    "FUNCALL_TODO"
    (*
    let finfo = get_env_func env func_id in
    let fidstr = get_id func_id in
    if finfo.f_defined then
      let f_args = finfo.f_args in
      let farg_len = List.length f_args in 
      let actual_len = List.length ex_list in
      if farg_len = actual_len then
        let s_ex_list = List.map2 (
          fun ex f_arg -> 
            (* check ex type must agree with expected arg type *)
            let _, s_ex, ex_type = gen_expr ex in 
            match ex_type, f_arg with
            | A.DataType(T.Int), A.DataType(T.Float)
            | A.DataType(T.Float), A.DataType(T.Int) -> s_ex
            | ex_type', f_arg' when ex_type' = f_arg' -> s_ex
            | _ -> failwith @@ "Incompatible args for function " ^fidstr^ ": "
                  ^ A.str_of_datatype ex_type ^ " given but " 
                  ^ A.str_of_datatype f_arg ^ " expected"   
          ) ex_list f_args
        in
        S.FunctionCall(fidstr, s_ex_list), finfo.f_return
      else
        failwith @@ "Function " ^fidstr^ " requires " ^ string_of_int farg_len
            ^ " arg but " ^ string_of_int actual_len ^ " provided"
    else
      failwith @@ if finfo.f_return = A.NoneType then
        "Function " ^ fidstr ^ " is undefined" else
        "Only forward declaration, no actual definition is found for " ^ fidstr
    *)
  
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
    
  | _ -> failwith "INTERNAL some expr not properly checked"


(*
and gen_s_array env exprs =
  let s_exprs, array_type = List.fold_left
    (* evaluate each expression in the list *)
    (fun (checked_exprs_acc, prev_type) unchecked_expr ->
        let checked_expr, expr_type = gen_expr unchecked_expr in
        match prev_type with
        | A.NoneType ->
          (* means we're seeing the 1st expr in the array and now know array type *)
          checked_expr :: checked_exprs_acc, expr_type
        | array_type -> (
          (* ensure all elems in array are the same type *)
          match array_type, expr_type with (* <> for != *)
          | A.DataType(T.Int), A.DataType(T.Float)
          | A.DataType(T.Float), A.DataType(T.Int) -> 
            checked_expr :: checked_exprs_acc, A.DataType(T.Float)
          | _ when array_type = expr_type -> 
            checked_expr :: checked_exprs_acc, array_type
          | _ ->  failwith @@ "Array element type conflict: " 
            ^ A.str_of_datatype array_type ^ " -.- " ^ A.str_of_datatype expr_type)
          )
    ([], A.NoneType) exprs in
  (List.rev s_exprs , array_type)

and gen_s_matrix env exprs_list_list =
  let matrix, matrix_type, row_length = List.fold_left
    (fun (rows, curr_type, row_length) exprs -> 
        (* evaluate each row where each row is an expr list *)
        let exprs, row_type = gen_s_array env exprs in
        let prev_type = 
            match row_type with 
            | A.DataType(prev_type) -> (
              match prev_type with
              | T.Int  | T.Float  | T.Complex -> prev_type
              | _ -> failwith @@ "Matrix element type unsupported: " ^ T.str_of_type prev_type
              )
            | _ -> failwith @@ "Invalid matrix row type: " ^ A.str_of_datatype row_type
        in
        let exprs_length = List.length exprs in
        match curr_type with
        | T.Void -> 
            (* means this is the 1st row which means we now know the matrix type *)
            (exprs :: rows), prev_type, exprs_length
        | _ -> (
          let curr_type' = 
            (* the same length*)
            if row_length <> exprs_length
            then failwith "All rows in a matrix must have the same length"
            else (
            (* ensure all rows have the same type and can only be complex, int or float *)
            match curr_type, prev_type with
            | T.Float, T.Int
            | T.Int, T.Float
            | T.Float, T.Float -> T.Float
            | T.Int, T.Int -> T.Int
            | T.Complex, T.Complex -> T.Complex
            | _ ->  failwith @@ 
              "Array element type conflict: " 
              ^ T.str_of_type curr_type ^ " -.- " ^ T.str_of_type prev_type
            )
            in
          (exprs :: rows), curr_type', row_length ))
    ([], T.Void, 0) exprs_list_list in
  (List.rev matrix , matrix_type, row_length)
  
    
(* decl *)
let rec check_matrix_decl idstr typ =
  match typ with
  | A.DataType(_) -> ()
  | A.ArrayType(t) -> check_matrix_decl idstr t
  | A.MatrixType(t) -> (
    match t with
    | A.DataType(mat_type) -> (
      match mat_type with
      (* only support 3 numerical types *)
      | T.Int | T.Float | T.Complex -> ()
      | _ -> failwith @@ 
        "Unsupported matrix element declaration: " ^idstr^ " with " ^ T.str_of_type mat_type)
    (* we shouldn't support float[][[]] *)
    | _ -> failwith @@ 
      "Invalid matrix declaration: " ^idstr^ " with " ^ A.str_of_datatype t
    )
  | A.NoneType -> failwith "INTERNAL NoneType encountered in check_matrix"
  
(* update_env_var checks redeclaration error *)
let gen_s_decl env = function
  | A.AssigningDecl(typ, id, ex) -> 
    let idstr = get_id id in
    let _ = check_matrix_decl idstr typ in (* disallow certain bad matrices *)
    let s_ex, ex_type = gen_expr ex in
    let _ = match typ, ex_type with
    | A.DataType(T.Int), A.DataType(T.Float)
    | A.DataType(T.Float), A.DataType(T.Int) -> ()
    | typ', ex_type' when typ' = ex_type' -> ()
    | _ -> failwith @@ "Incompatible assignment: " 
        ^ A.str_of_datatype typ ^" " ^idstr^ " = " ^ A.str_of_datatype ex_type
    in
    let env' = update_env_var env typ id in
    env', S.AssigningDecl(typ, idstr, s_ex)

  | A.PrimitiveDecl(typ, id) -> 
    let idstr = get_id id in
    let _ = check_matrix_decl idstr typ in (* disallow certain bad matrices *)
    let env' = update_env_var env typ id in
    env', S.PrimitiveDecl(typ, idstr)


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
      | _ -> failwith "INTERNAL codegen gen_param_list"
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
        ^ "} // end " ^ func_id ^ "()\n"
      
      | S.ForwardDecl(return_type, func_id, param_list) -> 
        let param_list_code = gen_param_list param_list in
        gen_datatype return_type ^ " " ^ func_id ^ "(" 
        ^ trim_last param_list_code ^ ");\n"

      (* statements *)
      | S.IfStatement(pred_ex, stmt_if, stmt_else) -> 
        ""
				
      | S.WhileStatement(pred_ex, stmt) -> 
        ""
        
      | S.ForStatement(iter, stmt) -> 
        ""
            
      | S.CompoundStatement(stmt_list) -> 
        let stmt_list_code = gen_code stmt_list in
        "{\n" ^ stmt_list_code ^ "} // end compound"

      | S.Declaration(dec) -> 
        ""

      | S.Expression(ex) -> (gen_expr ex) ^ ";"

      | S.ReturnStatement(ex) -> "return " ^ gen_expr ex ^ ";" 

      | S.VoidReturnStatement -> "return;"

      | S.BreakStatement -> "break;"

      | S.ContinueStatement -> "continue;"

      | S.EmptyStatement -> ";"

      | _ -> failwith "INTERNAL codegen unhandled statement"
    in 
    stmt_code ^ "\n" ^ gen_code rest