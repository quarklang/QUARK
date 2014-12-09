module A = Ast
module S = Sast
module T = Type

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
      (fun s typ -> s ^ (A.str_of_datatype typ) ^ ", ") "" f_args
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
      key ^ ": " ^ A.str_of_datatype vinfo.v_type ^ "(" ^ string_of_int vinfo.v_depth ^ "); ")
      env.var_table;
    print_string "\nFunc= ";
    StrMap.iter 
      (fun key finfo -> print_endline @@ 
        key ^ "(" ^ string_of_bool finfo.f_defined ^ "): " ^ 
        debug_s_decl_list finfo.f_args ^ " => " ^ A.str_of_datatype finfo.f_return ^ ";")
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
  let id = get_id var_id in
  try
    StrMap.find id env.var_table
  with Not_found -> 
    (* if the identifier appears as a func_id, then error! *)
    if StrMap.mem id env.func_table then
      failwith @@ "Confuse a function with a variable: " ^ id
    else
      { 
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
  | _ -> failwith @@ "Variable redeclaration: " ^ A.str_of_datatype var_typ ^ " " ^ get_id var_id

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


(*
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
*)

(********* Helpers for gen_s_expr ********)
(* Fraction, Qureg, Complex *)
let compound_type_err_msg name type1 type2 =
  "Invalid " ^ name ^ " literal operands: " ^ 
  A.str_of_datatype type1 ^","^ A.str_of_datatype type2

let is_matrix = function
  | A.MatrixType(_) -> true
  | _ -> false

(* Main expr semantic checker entry *)
(* return env', S.expr, type *)
let rec gen_s_expr env = function
  (* simple literals *)
  | A.IntLit(i) -> env, S.IntLit(i), A.DataType(T.Int)
  | A.BoolLit(b) -> env, S.BoolLit(b), A.DataType(T.Bool)
  | A.FloatLit(f) -> env, S.FloatLit(f), A.DataType(T.Float)
  | A.StringLit(s) -> env, S.StringLit(s), A.DataType(T.String)

  (* compound literals *)
  | A.FractionLit(num_ex, denom_ex) -> 
    let env, s_num_ex, num_type = gen_s_expr env num_ex in
    let env, s_denom_ex, denom_type = gen_s_expr env denom_ex in (
    match num_type, denom_type with
    | A.DataType(T.Int), A.DataType(T.Int) -> 
      env, S.FractionLit(s_num_ex, s_denom_ex), A.DataType(T.Fraction)
    | _ -> failwith @@ compound_type_err_msg "fraction" num_type denom_type
    )            

  | A.QRegLit(qex1, qex2) -> 
    let env, s_qex1, q1_type = gen_s_expr env qex1 in
    let env, s_qex2, q2_type = gen_s_expr env qex2 in (
    match q1_type, q2_type with
    | A.DataType(T.Int), A.DataType(T.Int) -> 
      env, S.QRegLit(s_qex1, s_qex2), A.DataType(T.QReg)
    | _ -> failwith @@ compound_type_err_msg "qreg" q1_type q2_type
    )            

  | A.ComplexLit(real_ex, im_ex) -> 
    let env, s_real_ex, real_type = gen_s_expr env real_ex in
    let env, s_im_ex, im_type = gen_s_expr env im_ex in (
    match real_type, im_type with
    | A.DataType(T.Int), A.DataType(T.Int)
    | A.DataType(T.Int), A.DataType(T.Float)
    | A.DataType(T.Float), A.DataType(T.Int)
    | A.DataType(T.Float), A.DataType(T.Float) -> 
      env, S.ComplexLit(s_real_ex, s_im_ex), A.DataType(T.Complex)
    | _ -> failwith @@ compound_type_err_msg "complex" real_type im_type
    )            

  | A.ArrayLit(exprs) ->
    let env, s_exprs, elem_type = gen_s_array env exprs in
    let _tmp = A.ArrayType(elem_type) in
    let _ = print_endline @@ "ARRAY " ^ (A.str_of_datatype _tmp) in
    env, S.ArrayLit(elem_type, s_exprs), A.ArrayType(elem_type)

  | A.MatrixLit(exprs_list_list) ->
    let env, s_matrix, elem_type = gen_s_matrix env exprs_list_list in
    let _tmp = A.MatrixType(A.DataType(elem_type)) in
    let _ = print_endline @@ "MATRIX " ^ (A.str_of_datatype _tmp) in
    env, S.MatrixLit(elem_type, s_matrix), A.MatrixType(A.DataType(elem_type))
  
  (* Binary ops *)
  (* '+' used for matrix addition, '&' for array concatenation *)
  | A.Binop(expr1, op, expr2) -> 
    (* helper functions for Binop case *)
    let err_msg_helper func_str_of op type1 type2 =
      "Incompatible operands for binary op " ^ A.str_of_binop op ^ ": " 
      ^ func_str_of type1 ^ " -.- " ^ func_str_of type2 in
    let err_msg = err_msg_helper T.str_of_type in (* basic types *)
    let err_msg_arrmat = err_msg_helper A.str_of_datatype in (* array/matrix types *)
    
    (* check left and right children *)
    let env, s_expr1, ex_type1 = gen_s_expr env expr1 in
    let env, s_expr2, ex_type2 = gen_s_expr env expr2 in
    begin
    match ex_type1, ex_type2 with 
    (* cases with raw types *)
    | A.DataType(type1), A.DataType(type2) -> 
      begin
      let logic_relational op type1 type2 =
        match type1, type2 with
          | T.Int,   T.Int 
          | T.Float, T.Float 
          | T.Int,   T.Float 
          | T.Float, T.Int -> T.Bool, S.OpVerbatim
          (* | T.Fraction, T.Fraction -> T.Bool *)
          | t1, t2 -> failwith @@ err_msg op t1 t2
      in
      let binop_math op type1 type2 = 
          let notmod = op <> A.Mod in
          let notmodpow = notmod && op <> A.Pow in
          match type1, type2 with
          | T.Float, T.Int
          | T.Int,   T.Float 
          | T.Float, T.Float when notmod -> 
              T.Float, S.OpVerbatim
          | T.Int,   T.Int -> 
              T.Int, S.OpVerbatim
          | T.Float, T.Complex when notmod -> 
              T.Complex, S.CastComplex1
          | T.Complex, T.Float when notmod -> 
              T.Complex, S.CastComplex2
          | T.Complex, T.Complex when notmod -> 
              T.Complex, S.OpVerbatim
          | T.Int, T.Fraction when notmodpow -> 
              T.Fraction, S.CastFraction1
          | T.Fraction, T.Int when notmodpow -> 
              T.Fraction, S.CastFraction2
          | T.Fraction, T.Fraction  when notmodpow ->
              T.Fraction, S.OpVerbatim
          | t1, t2 -> failwith @@ err_msg op t1 t2
      in
      let logic_basic op type1 type2 =
        match type1, type2 with
          | T.Bool, T.Bool -> T.Bool, S.OpVerbatim
          | t1, t2 -> failwith @@ err_msg op t1 t2
      in
      let logic_equal op type1 type2 = 
        match type1, type2 with
          | T.Float, T.Int
          | T.Int,   T.Float -> T.Bool, S.OpVerbatim
          | t1, t2 when t1 = t2 -> T.Bool, S.OpVerbatim
          | t1, t2 -> failwith @@ err_msg op t1 t2
      in
      let binop_bitwise op type1 type2 = 
        match type1, type2 with
          | T.Int, T.Int -> T.Int, S.OpVerbatim
          | T.String, T.String when op = A.BitAnd -> 
              T.String, S.OpStringConcat
          | t1, t2 -> failwith @@ err_msg op t1 t2
      in
      let result_type, optag = 
        match op with 
          | A.Add         -> binop_math op type1 type2
          | A.Sub         -> binop_math op type1 type2
          | A.Mul         -> binop_math op type1 type2
          | A.Div         -> binop_math op type1 type2
          | A.Pow         -> binop_math op type1 type2
          | A.Mod         -> binop_math op type1 type2
          | A.Eq          -> logic_equal op type1 type2 
          | A.NotEq       -> logic_equal op type1 type2
          | A.Less        -> logic_relational op type1 type2 
          | A.LessEq      -> logic_relational op type1 type2
          | A.Greater     -> logic_relational op type1 type2
          | A.GreaterEq   -> logic_relational op type1 type2
          | A.And         -> logic_basic op type1 type2
          | A.Or          -> logic_basic op type1 type2
          | A.BitAnd      -> binop_bitwise op type1 type2
          | A.BitOr       -> binop_bitwise op type1 type2
          | A.BitXor      -> binop_bitwise op type1 type2
          | A.Lshift      -> binop_bitwise op type1 type2
          | A.Rshift      -> binop_bitwise op type1 type2
          | _ -> failwith "INTERNAL unmatched binop"
      in
      env, S.Binop(s_expr1, op, s_expr2, optag), A.DataType(result_type)
      end
      
    (* At least one of the binop operand is an array/matrix *)
    | type1, type2 -> 
      let result_type, optag =
        match op with
        | A.Eq  | A.NotEq when type1 = type2 -> 
            A.DataType(T.Bool), S.OpVerbatim
        | A.Add | A.Sub | A.Mul | A.Pow when type1 = type2 && is_matrix type1 -> 
            (* matrix pow will be kronecker product *)
            type1, S.OpMatrixMath (* matrix operations *)
        | A.BitAnd when type1 = type2 -> 
            type1, S.OpArrayConcat (* array/mat concatenation *)
        | _ -> failwith @@ err_msg_arrmat op type1 type2
      in
      env, S.Binop(s_expr1, op, s_expr2, optag), result_type
    end (* end of binop *)
    
  (* Query ops *)
  | A.Queryop(qreg_ex, op, start_ex, end_ex) -> 
    let env, s_qreg_ex, qreg_type = gen_s_expr env qreg_ex in
    begin
      match qreg_type with 
      | A.DataType(T.QReg) ->
        let env, s_start_ex, start_type = gen_s_expr env start_ex in
        let env, s_end_ex, end_type = gen_s_expr env end_ex in
        let optag = match s_end_ex with
          (* dummy literal from parser *)
        | S.IntLit("QuerySingleBit") -> S.OpQuerySingleBit
        | _ -> S.OpVerbatim in (
        match start_type, end_type with
        | A.DataType(T.Int), A.DataType(T.Int) -> 
          (* query check success *)
          env, S.Queryop(s_qreg_ex, op, s_start_ex, s_end_ex, optag), A.DataType(T.Int)
        | _ -> failwith @@ "Incompatible query args: " ^ A.str_of_datatype start_type 
          ^ (if optag = S.OpVerbatim then ", " ^ A.str_of_datatype end_type else "")
        )
      | _ -> failwith @@ 
          "Measurement must be queried on a qureg, not " ^ A.str_of_datatype qreg_type
    end
    
  (* Unary ops *)
  | A.Unop(op, ex) -> 
    let env, s_ex, typ = gen_s_expr env ex in
    let err_msg op t = "Incompatible operand for unary op " 
        ^ A.str_of_unop op ^ ": " ^ A.str_of_datatype t in
    let return_type = 
      if is_matrix typ && op = A.Neg then 
        typ (* matrix support negation *)
      else
      A.DataType(
        let raw_type = match typ with
          | A.DataType(t) -> t
          | _ -> failwith @@ err_msg op typ 
        in
        match op with
        | A.Neg -> (match raw_type with
          | T.Int | T.Float | T.Fraction | T.Complex -> raw_type
          | _ -> failwith @@ err_msg op typ)
        | A.Not -> (match raw_type with
          | T.Bool -> T.Bool
          | _ -> failwith @@ err_msg op typ)
        | A.BitNot -> (match raw_type with
           (* ~fraction inverts the fraction *)
          | T.Int | T.Fraction -> raw_type
          | _ -> failwith @@ err_msg op typ)
      )
    in
    env, S.Unop(op, s_ex, S.OpVerbatim), return_type
  
  (* Special assignment *)
  | A.AssignOp(lval, op, ex) -> 
    env, S.IntLit("TODO"), A.DataType(T.Int)
      (*
    gen_lvalue lval ^" "^ gen_binop op ^" "^ gen_s_expr ex
      *)

  | A.PostOp(lval, op) -> 
    env, S.IntLit("TODO"), A.DataType(T.Int)
      (*
    gen_lvalue lval ^" "^ gen_postop op
      *)
    
  | A.Lval(lval) -> 
    env, S.IntLit("TODO"), A.DataType(T.Int)
      (*
    gen_lvalue lval
      *)
  
  (* Assignment *)
  | A.Assign(lval, ex) -> 
    env, S.IntLit("TODO"), A.DataType(T.Int)
      (*
    gen_lvalue lval ^ " = " ^ gen_s_expr ex
      *)

  (* Membership testing with keyword 'in' *)
  | A.Membership(exElem, exArray) -> 
    env, S.IntLit("TODO"), A.DataType(T.Int)
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
    env, S.IntLit("TODO"), A.DataType(T.Int)
      (*
    get_id funcId ^ surr( gen_expr_list exlist )
      *)
  
  | _ -> failwith "INTERNAL some expr not properly checked"


and gen_s_array env exprs =
  let env, s_exprs, array_type = List.fold_left
    (* evaluate each expression in the list *)
    (fun (env, checked_exprs_acc, prev_type) unchecked_expr ->
        let env, checked_expr, expr_type = gen_s_expr env unchecked_expr in
        match prev_type with
        | A.NoneType ->
          (* means we're seeing the 1st expr in the array and now know array type *)
          env, checked_expr :: checked_exprs_acc, expr_type
        | array_type -> (
          (* ensure all elems in array are the same type *)
          match array_type, expr_type with (* <> for != *)
          | A.DataType(T.Int), A.DataType(T.Float)
          | A.DataType(T.Float), A.DataType(T.Int) -> 
            env, checked_expr :: checked_exprs_acc, A.DataType(T.Float)
          | _ when array_type = expr_type -> 
            env, checked_expr :: checked_exprs_acc, array_type
          | _ ->  failwith @@ 
            "Array element type conflict: " 
            ^ A.str_of_datatype array_type ^ " -.- " ^ A.str_of_datatype expr_type)
          )
    (env, [], A.NoneType) exprs in
  (env, List.rev s_exprs , array_type)

and gen_s_matrix env exprs_list_list =
  let env, matrix, matrix_type, _ = List.fold_left
    (fun (env, rows, curr_type, row_length) exprs -> 
        (* evaluate each row where each row is an expr list *)
        let env, exprs, row_type = gen_s_array env exprs in
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
            env, (exprs :: rows), prev_type, exprs_length

        | _ -> (
          let curr_type' = 
            (* the same length*)
            if row_length <> exprs_length
            then failwith "All rows in a matrix must be the same length"
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
          env, (exprs :: rows), curr_type', row_length ))
    (env, [], T.Void, 0) exprs_list_list in
  (env, List.rev matrix , matrix_type)
  
(*
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
            
      | A.CompoundStatement(stmt_list) -> 
        let env' = incr_env_depth env in
        (env, S.CompoundStatement(snd_2 @@ gen_sast env' stmt_list))

      | A.Declaration(dec) -> 
        let env', s_dec = gen_s_decl env dec in
        let _ = debug_env env' "after decl" in
        (env', S.Declaration(s_dec))

      | A.Expression(ex) -> 
        let env', s_ex, _ = gen_s_expr env ex in
        (env', S.Expression(s_ex))

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
