module A = Ast
module S = Sast
module T = Type

module StrMap = Map.Make(String)

(* debug printout flag *)
let _DEBUG_ENABLE = false

(* utilities *)
let fst_2 = function x, _ -> x;;
let snd_2 = function _, x -> x;;
let fst_3 = function x, _, _ -> x;;
let snd_3 = function _, x, _ -> x;;
let trd_3 = function _, _, x -> x;;

let get_id (A.Ident name) = 
  (* reserved prefix *)
  let forbid = Builtin.forbidden_prefix in
  let forbid_len = String.length forbid in
  if String.length name < forbid_len then name
  else
    if String.sub name 0 forbid_len = forbid
    then failwith @@ "Identifier name cannot start with "
        ^ "the reserved prefix " ^forbid^ ": " ^ name
    else name

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
    is_returned: bool;
    in_loop: bool;  (* check break/continue validity *)
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
  if _DEBUG_ENABLE then
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
    print_endline @@ "is_returned= " ^ string_of_bool env.is_returned;
    print_endline "}";
  end

let debug_print msg = 
  if _DEBUG_ENABLE then print_endline @@ "DEBUG " ^ msg


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
      failwith @@ "A function is confused with a variable: " ^ id
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
    { env with var_table = update_var_table env var_typ var_id }
  | _ -> failwith @@ "Variable redeclaration: " 
      ^ A.str_of_datatype var_typ ^ " " ^ get_id var_id

(* go one scope deeper *)
let incr_env_depth env = 
  { env with depth = env.depth + 1 }
(* go one scope shallower *)
let decr_env_depth env = 
  { env with depth = env.depth - 1 }

let set_env_returned env = 
  { env with is_returned = true }


(****** Environment func_table ******)
let get_env_func env func_id =
  let fid = get_id func_id in
  try
    StrMap.find fid env.func_table
  with Not_found -> 
    (* look at the built-in functions *)
    let arg_types, return_type = Builtin.find_builtin fid in
    { 
      f_args = arg_types; 
      f_return = return_type;
      f_defined = return_type <> A.NoneType;
    }
  
(* Used in A.FunctionDecl *)
(* add all formal params to updated var_table *)
let update_env_func env return_type func_id s_param_list is_defined =
  let finfo = get_env_func env func_id in
  let errmsg_str = ": " ^ get_id func_id ^ "()" in
  let s_arg_types = 
    List.map (function
        | A.PrimitiveDecl(typ, id) -> typ
        | _ -> failwith @@ "Function parameter list declaration error" ^ errmsg_str
        ) s_param_list in
  (* add formal params to var scope. This is a lambda function *)
  let add_formal_var_lambda = 
    List.fold_left 
        (fun env -> function
        | A.PrimitiveDecl(typ, id) -> 
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
      is_returned = not is_defined; (* if not forward decl, we need to return *)
      in_loop = false;
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


(******* Helpers for gen_s_expr ******)
(* Fraction, Qureg, Complex *)
let compound_type_err_msg name type1 type2 =
  "Invalid " ^ name ^ " literal operands: " ^ 
  A.str_of_datatype type1 ^","^ A.str_of_datatype type2

let is_matrix = function
  | A.MatrixType(_) -> true
  | _ -> false

let is_lvalue = function
  | A.Lval(_) -> true
  | _ -> false

(* flatten a matrix (list list) into row major 1D list *)
let flatten_matrix = List.fold_left
  (fun acc row -> acc @ row ) []


(********* Main expr semantic checker entry *********)
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
      env, S.QRegLit(s_qex1, s_qex2), A.DataType(T.Qreg)
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
    let arr_type = A.ArrayType(elem_type) in
    let _ = debug_print @@ "ARRAY " ^ (A.str_of_datatype arr_type) in
    env, S.ArrayLit(arr_type, s_exprs), arr_type
    
  (* constructs an array with size *)
  | A.ArrayCtor(elem_type, size_expr) -> 
    let env, s_size_expr, size_type = gen_s_expr env size_expr in
    if size_type = A.DataType(T.Int) then
      let arr_type = A.ArrayType(elem_type) in
      env, S.ArrayCtor(arr_type, s_size_expr), arr_type
    else
      failwith @@ "Array constructor size must be int, but " 
            ^ A.str_of_datatype size_type ^ " provided"

  | A.MatrixLit(exprs_list_list) ->
    let env, s_matrix, elem_type, coldim = gen_s_matrix env exprs_list_list in
    let elem_type = A.DataType(elem_type) in
    let _ = debug_print @@ "MATRIX " ^ A.str_of_datatype (A.MatrixType(elem_type)) 
        ^ " cols= " ^ string_of_int coldim ^ " rows= " ^ string_of_int (List.length exprs_list_list) 
    in
    env, S.MatrixLit(elem_type, s_matrix, coldim), A.MatrixType(elem_type)
  
  (* constructs a matrix with row, col dim *)
  | A.MatrixCtor(elem_type, rowdim_ex, coldim_ex) -> (
    match elem_type with
    | A.DataType(t) -> 
      if t = T.Int || t = T.Float || t = T.Complex then
        let env, s_rowdim_ex, rowdim_type = gen_s_expr env rowdim_ex in
        let env, s_coldim_ex, coldim_type = gen_s_expr env coldim_ex in
        if rowdim_type = A.DataType(T.Int) && coldim_type = A.DataType(T.Int) then
          let mat_type = A.MatrixType(elem_type) in
          env, S.MatrixCtor(mat_type, s_rowdim_ex, s_coldim_ex), mat_type
        else
          failwith @@ "Matrix constructor row/column dimensions must be int/int, but " 
                ^ A.str_of_datatype rowdim_type ^ "/"
                ^ A.str_of_datatype coldim_type ^ " provided"
      else
        failwith @@ 
          "Non-numerical matrix constructor type: " ^ A.str_of_datatype elem_type
    | _ -> failwith @@ 
        "Invalid matrix constructor type: " ^ A.str_of_datatype elem_type
    )

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
          | T.Float, T.Int 
          | T.Fraction, T.Fraction
          | T.String, T.String -> T.Bool, S.OpVerbatim
          | T.Int, T.Fraction
          | T.Float, T.Fraction -> T.Bool, S.CastFraction1
          | T.Fraction, T.Int
          | T.Fraction, T.Float -> T.Bool, S.CastFraction2
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
          | T.Float, T.Complex
          | T.Int, T.Complex when notmod -> 
              T.Complex, S.CastComplex1
          | T.Complex, T.Float
          | T.Complex, T.Int when notmod -> 
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
            type1, if op = A.Pow then S.OpMatrixKronecker else S.OpVerbatim
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
      | A.DataType(T.Qreg) ->
        (* we disallow measurement on an rvalue, e.g. a qureg literal *)
        let _ = if not (is_lvalue qreg_ex) then
            failwith "Measurement query on a Qreg must be made on lvalue type"
        in
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
    let return_type, optag = 
      if is_matrix typ then 
        (* matrix support negation and transposition *)
        let optag = match op with
        | A.Neg -> S.OpVerbatim
        | A.Transpose -> S.OpMatrixTranspose
        | _ -> failwith @@ err_msg op typ
        in
        typ, optag 
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
        | _ -> failwith @@ err_msg op typ
      ), S.OpVerbatim
    in
    env, S.Unop(op, s_ex, optag), return_type
  
  | A.Lval(lval) -> 
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
            let _, s_ex, typ = gen_s_expr env ex in
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
              failwith @@ "Invalid subscript dimension for array: " ^idstr
            | A.ArrayType(elem') ->
              get_array_lval_type (sub_dim - 1) elem'
              (* assume decl has already checked that matrix type is valid *)
            | A.MatrixType(A.DataType(raw_elem)) ->
              if sub_dim = 2 then A.DataType(raw_elem)
              else failwith @@ 
                  "Invalid subscript dimension for array that contains matrix: " ^idstr
            | _ -> failwith @@ "INTERNAL bad array type: " ^ idstr
          in
          let lval_type = get_array_lval_type (sub_dim - 1) elem_type in
          let _ = debug_print @@ "LVALUE "^idstr^" -> "^A.str_of_datatype lval_type in
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
    env, S.Lval(s_lval), ltype
    
  (* Special assignment *)
  | A.AssignOp(lval, op, ex) ->
    let binop = match op with
    | A.AddEq -> A.Add
    | A.SubEq -> A.Sub
    | A.MulEq -> A.Mul
    | A.DivEq -> A.Div
    | A.BitAndEq -> A.BitAnd
    | _ -> failwith @@ "INTERNAL bad AssignOp: " ^ A.str_of_binop op
    in
    gen_s_expr env (A.Assign(lval, A.Binop(A.Lval(lval), binop, ex)))

  (* Post ++ and -- *)
  | A.PostOp(lval, op) -> 
    let env, s_lval_ex, typ = gen_s_expr env (A.Lval(lval)) in
      let s_lval = match s_lval_ex with
      | S.Lval(s_lval) -> s_lval
      | _ -> failwith "INTERNAL in postop: doesn't return S.Lval as expected"
      in (
      match typ with
      | A.DataType(T.Int)  | A.DataType(T.Float) -> 
        env, S.PostOp(s_lval, op), typ
      | _ -> failwith @@ "Incompatible operand for post op " 
          ^ A.str_of_postop op ^ ": " ^ A.str_of_datatype typ
      )
      
  (* Assignment *)
  | A.Assign(lval, rhs_ex) -> 
    let env, s_lval_ex, l_type = gen_s_expr env (A.Lval(lval)) in
    let env, s_rhs_ex, r_type = gen_s_expr env rhs_ex in
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
      let _ = debug_print @@ "ASSIGN returns "^A.str_of_datatype return_type in
      env, S.Assign(s_lval, s_rhs_ex), return_type

  (* Function calls *)
  | A.FunctionCall(func_id, ex_list) -> 
    let finfo = get_env_func env func_id in
    let fidstr = get_id func_id in
    if Builtin.is_print fidstr then
      (* 'print' built-in functions support any number of args *)
      (* We keep a bool list of whether each arg is a matrix. For eigen prettyprint *)
      let s_ex_list, is_matrix_list = 
        List.fold_right ( (* fold right so we don't have to List.rev *)
          fun ex (s_ex_list, is_matrix_list) -> 
            let _, s_ex, ex_type = gen_s_expr env ex in
            s_ex :: s_ex_list, (is_matrix ex_type) :: is_matrix_list
        ) ex_list ([], [])
      in
      env, S.FunctionCall(fidstr, s_ex_list, is_matrix_list), finfo.f_return
    else (* non-special cases *)
    if finfo.f_defined then
      let f_args = finfo.f_args in
      let farg_len = List.length f_args in 
      let actual_len = List.length ex_list in
      if farg_len = actual_len then
        let s_ex_list = List.map2 (
          fun ex f_arg -> 
            (* check ex type must agree with expected arg type *)
            let _, s_ex, ex_type = gen_s_expr env ex in 
            match ex_type, f_arg with
            | A.DataType(T.Int), A.DataType(T.Float)
            | A.DataType(T.Float), A.DataType(T.Int)
               (* Array(None) means built-in function matches any array type *)
            | A.ArrayType(_), A.ArrayType(A.NoneType)
            | A.MatrixType(_), A.MatrixType(A.NoneType) -> s_ex
            | ex_type', f_arg' when ex_type' = f_arg' -> 
              if ex_type = A.DataType(T.Qreg) then
                (* disallow non-lvalue qureg to be used as function parameter *)
                if is_lvalue ex then s_ex
                else failwith @@
                  "Qreg parameter to function "^fidstr^"() must be lvalue type"
              else s_ex
            | _ -> failwith @@ "Incompatible args for function " ^fidstr^ ": "
                  ^ A.str_of_datatype ex_type ^ " given but " 
                  ^ A.str_of_datatype f_arg ^ " expected"   
          ) ex_list f_args
        in
        (* check apply_oracle: arg#2(string) must represent a function(int) returns int *)
        let _ = if fidstr = "apply_oracle" then
          let oracle_ex = List.nth ex_list 1 in
          let oracle_id = match oracle_ex with
          | A.StringLit(id) -> id
          | _ -> failwith "Arg #2 of built-in apply_oracle() must be a string literal"
          in
          let oracle_finfo = get_env_func env (A.Ident(oracle_id)) in
          if oracle_finfo.f_args <> [A.DataType(T.Int)]
            || oracle_finfo.f_return <> A.DataType(T.Int) then
            failwith @@ "Arg #2 of built-in apply_oracle(): user-defined function " 
              ^ oracle_id ^" must have signature 'int " ^ oracle_id ^ "(int)'"
        in
        env, S.FunctionCall(fidstr, s_ex_list, []), finfo.f_return
      else
        failwith @@ "Function " ^fidstr^ " requires " ^ string_of_int farg_len
            ^ " arg but " ^ string_of_int actual_len ^ " provided"
    else
      failwith @@ if finfo.f_return = A.NoneType then
        "Function " ^ fidstr ^ " is undefined" else
        "Only forward declaration, no actual definition is found for " ^ fidstr
  
  (* Membership testing with keyword 'in' *)
  | A.Membership(elem, array_ex) -> 
    let env, s_array_ex, array_type = gen_s_expr env array_ex in
    let env, s_elem, elem_type = gen_s_expr env elem in
    let arr_elem_type = match array_type with 
      | A.ArrayType(elem_type) -> elem_type
      | _ -> failwith @@ 
        "Membership testing must operate on array type, not " ^ A.str_of_datatype array_type
    in
    let _ = match elem_type, arr_elem_type with
      | A.DataType(T.Int), A.DataType(T.Float)
      | A.DataType(T.Float), A.DataType(T.Int) -> ()
      | elem_type', arr_elem_type' when elem_type' = arr_elem_type' -> ()
      | _ -> failwith @@ "Membership testing has incompatible types: " 
          ^ A.str_of_datatype elem_type ^" -.- "^ A.str_of_datatype arr_elem_type
    in
    env, S.Membership(s_elem, s_array_ex), A.DataType(T.Bool)
  
  (* Python style tertiary *)
  | A.Tertiary(true_ex, pred, false_ex) -> 
    let env, s_pred, pred_type = gen_s_expr env pred in
    if pred_type = A.DataType(T.Bool) then
      let env, s_true_ex, true_type = gen_s_expr env true_ex in
      let env, s_false_ex, false_type = gen_s_expr env false_ex in
      let result_type, optag = match true_type, false_type with
      | A.DataType(t), A.DataType(f) -> 
        let ret, optag = match t,f with
        | T.Int, T.Float
        | T.Float, T.Int -> T.Float, S.OpVerbatim
        | T.Int, T.Fraction
        | T.Float, T.Fraction -> T.Fraction, S.CastFraction1
        | T.Fraction, T.Int
        | T.Fraction, T.Float -> T.Fraction, S.CastFraction2
        | T.Int, T.Complex
        | T.Float, T.Complex -> T.Complex, S.CastComplex1
        | T.Complex, T.Int
        | T.Complex, T.Float -> T.Complex, S.CastComplex2
        | t', f' when t' = f' -> t', S.OpVerbatim
        | _ -> failwith @@ "Tertiary expression has incompatible types: " 
                  ^ T.str_of_type t ^" -.- "^ T.str_of_type f
        in A.DataType(ret), optag
      | true', false' when true' = false' -> true', S.OpVerbatim
      | _ -> failwith @@ "Tertiary expression has incompatible types: " 
          ^ A.str_of_datatype true_type ^" -.- "^ A.str_of_datatype false_type
      in
      env, S.Tertiary(s_true_ex, s_pred, s_false_ex, optag), result_type
    else
      failwith @@ 
        "Tertiary predicate must be bool, but "^ A.str_of_datatype pred_type ^" provided"
    
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
          | _ ->  failwith @@ "Array element type conflict: " 
            ^ A.str_of_datatype array_type ^ " -.- " ^ A.str_of_datatype expr_type)
          )
    (env, [], A.NoneType) exprs in
  (env, List.rev s_exprs , array_type)

and gen_s_matrix env exprs_list_list =
  let env, matrix, matrix_type, row_length = List.fold_left
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
          env, (exprs :: rows), curr_type', row_length ))
    (env, [], T.Void, 0) exprs_list_list 
  in
  let matrix = flatten_matrix (List.rev matrix) in
  env, matrix , matrix_type, row_length
  

let gen_s_param = function 
  | A.PrimitiveDecl(typ, id) -> 
    S.PrimitiveDecl(typ, get_id id)
  | _ -> failwith "Function parameter list declaration error"

(* Used in A.FunctionDecl *)
let gen_s_param_list param_list =
  List.map 
    (fun param -> gen_s_param param) param_list
   
     
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
      | _ -> failwith @@ "Non-numerical matrix declaration: " 
            ^ idstr ^ " with " ^ T.str_of_type mat_type)
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
    let env, s_ex, ex_type = gen_s_expr env ex in
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
      let env, s_start_ex, start_type = gen_s_expr env start_ex in
      let env, s_end_ex, end_type = gen_s_expr env end_ex in
      let env, s_step_ex, step_type = gen_s_expr env step_ex in
      match start_type, end_type, step_type with
      | A.DataType(start_raw_typ), A.DataType(end_raw_typ), A.DataType(step_raw_typ) -> 
        if not (start_raw_typ = T.Float || start_raw_typ = T.Int) ||
           not (end_raw_typ = T.Float || end_raw_typ = T.Int) ||
           not (step_raw_typ = T.Float || step_raw_typ = T.Int) then
           failwith @@ "Unsupported range type: " ^ T.str_of_type start_raw_typ ^ ", "
               ^ T.str_of_type end_raw_typ ^ ", " ^ T.str_of_type step_raw_typ
        else
           S.Range(vtype, s_start_ex, s_end_ex, s_step_ex)
      | _ -> failwith @@ "Unsupported range type: " ^ A.str_of_datatype start_type ^ ", "
               ^ A.str_of_datatype end_type ^ ", " ^ A.str_of_datatype step_type
      end
    | _ -> failwith @@ "Unsupported for-iterator " ^idstr^ ": " ^A.str_of_datatype vtype

let gen_s_iter env = function
  | A.RangeIterator(typ, id, range) -> (
    (* if typ = NoneType, there's no new iterator variable defined in the loop *)
    match typ with
    | A.NoneType ->
      env, S.RangeIterator(typ, get_id id, gen_s_range env id range)
    | _ -> 
      (* add the declared var to scope *)
      let env', _ = gen_s_decl env (A.PrimitiveDecl(typ, id)) in
      env', S.RangeIterator(typ, get_id id, gen_s_range env' id range)
    )

  | A.ArrayIterator(typ, id, array_ex) -> 
    let idstr = get_id id in
    let env', s_array_ex, array_type = gen_s_expr env array_ex in
    let env', _ = gen_s_decl env (A.PrimitiveDecl(typ, id)) in
    let elem_type = match array_type with 
      | A.ArrayType(elem_type) -> elem_type
      | _ -> failwith @@ 
        "Array-style for-loop must operate on array type, not " ^ A.str_of_datatype array_type
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


(********** Main entry point: AST -> SAST **********)
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
        let fidstr = get_id func_id in
        (* check: mustn't override certain built-in functions *)
        let _ = Builtin.overridable fidstr in
        (* check: main function must have void main() signature *)
        let _ = if fidstr = "main" then
          if List.length param_list > 0 
            || return_type <> A.DataType(T.Int) then
          failwith "Main entry function must have signature 'int main()'"
        in
        let env' = incr_env_depth env in
        let env' = update_env_func env' return_type func_id param_list true in
        let _ = debug_env env' "after FunctionDecl" in
        (* get the function declaration, then close 'func_current' *)
        let env_after_decl, s_stmt_list = gen_sast env' stmt_list in
        (* check if properly returned *)
        let is_returned = env_after_decl.is_returned in
        let _ = if is_returned then ()
          else if return_type = A.DataType(T.Void) then ()
          else failwith @@ "Function " ^ fidstr 
              ^ " should have at least one return: " ^ A.str_of_datatype return_type
        in
        let function_decl = 
          S.FunctionDecl(return_type, get_id func_id, s_param_list, s_stmt_list) in
        let env'' = { 
          var_table = env.var_table; 
          func_table = env'.func_table;
          func_current = "";
          depth = env.depth;
          is_returned = true;
          in_loop = false;
        } in
        let _ = debug_env env'' "closed after FuncDecl" in
        env'', function_decl
      
      | A.ForwardDecl(return_type, func_id, param_list) -> 
        let fidstr = get_id func_id in
        (* check: mustn't override certain built-in functions *)
        let _ = Builtin.overridable fidstr in
        (* check: cannot forward decl main function *)
        let _ = if fidstr = "main" then
          failwith "Cannot forward declare main()"
        in
        let s_param_list = gen_s_param_list param_list in
        let env' = update_env_func env return_type func_id param_list false in
        env', S.ForwardDecl(return_type, fidstr, s_param_list)

      (* statements *)
      | A.IfStatement(pred_ex, stmt_if, stmt_else) -> 
        let env', s_pred_ex, pred_type = gen_s_expr env pred_ex in
        if pred_type = A.DataType(T.Bool) then
          let env_if = handle_compound_env env' stmt_if in
          let env_if, s_stmt_if = gen_sast env_if [stmt_if] in
          let env_else = handle_compound_env env' stmt_else in
          let env_else, s_stmt_else = gen_sast env_else [stmt_else] in
          let env = 
            if env_if.is_returned || env_else.is_returned 
            then set_env_returned env else env in
          env, S.IfStatement(s_pred_ex, List.hd s_stmt_if, List.hd s_stmt_else)
        else
          failwith @@ "If predicate must be bool, but " 
              ^ A.str_of_datatype pred_type ^ " provided"
				
      | A.WhileStatement(pred_ex, stmt) -> 
        let env', s_pred_ex, pred_type = gen_s_expr env pred_ex in
        let env' = handle_compound_env env' stmt in
        if pred_type = A.DataType(T.Bool) then
          let env' = { env' with in_loop = true } in
          let env', s_stmt = gen_sast env' [stmt] in
          let env = 
            if env'.is_returned then set_env_returned env else env in
          env, S.WhileStatement(s_pred_ex, List.hd s_stmt)
        else
          failwith @@ "While predicate must be bool, but " 
              ^ A.str_of_datatype pred_type ^ " provided"
            
      | A.ForStatement(iter, stmt) -> 
        (* hack: first go one scope deeper, then go back to ensure that*)
        (* the iterator variable is in the right scope *)
        let env' = incr_env_depth env in
        let env', s_iter = gen_s_iter env' iter in
        let env' = decr_env_depth env' in
        let env' = handle_compound_env env' stmt in
        let env' = { env' with in_loop = true } in
        let env', s_stmt = gen_sast env' [stmt] in
        let env = 
          if env'.is_returned then set_env_returned env else env in
        env, S.ForStatement(s_iter, List.hd s_stmt)
            
      | A.CompoundStatement(stmt_list) -> 
        let env' = incr_env_depth env in
        let env', s_stmt_list = gen_sast env' stmt_list in
        let env = 
          if env'.is_returned then set_env_returned env else env in
        env, S.CompoundStatement(s_stmt_list)

      | A.Declaration(dec) -> 
        let env', s_dec = gen_s_decl env dec in
        let _ = debug_env env' "after decl" in
        env', S.Declaration(s_dec)

      | A.Expression(ex) -> 
        let env', s_ex, _ = gen_s_expr env ex in
        env', S.Expression(s_ex)

      | A.ReturnStatement(ex) -> 
        if env.func_current = "" then
          failwith @@ "Invalid return statement outside function definition"
        else
          let f_return = 
            (get_env_func env (A.Ident(env.func_current))).f_return in
          let _, s_ex, return_type = gen_s_expr env ex in
          let s_ex = match f_return, return_type with
          | A.DataType(T.Int), A.DataType(T.Float)
          | A.DataType(T.Float), A.DataType(T.Int) -> s_ex
          | f_return', return_type' when f_return' = return_type' -> s_ex
          | _ -> failwith @@ "Function " ^env.func_current 
              ^ " should return " ^ A.str_of_datatype f_return 
              ^ ", not " ^ A.str_of_datatype return_type
          in
          let env' = set_env_returned env in
          env', S.ReturnStatement(s_ex)

      | A.VoidReturnStatement -> 
        if env.func_current = "" then
          failwith @@ "Invalid return statement outside function definition"
        else
          let f_return = 
            (get_env_func env (A.Ident(env.func_current))).f_return in
          if f_return = A.DataType(T.Void) then
            let env' = set_env_returned env in
            env', S.VoidReturnStatement
          else
            failwith @@ "Function " ^env.func_current 
              ^ " should return " ^ A.str_of_datatype f_return ^ ", not void"

      | A.BreakStatement -> 
        if env.in_loop then
          env, S.BreakStatement
        else failwith "Invalid break statement outside a loop"

      | A.ContinueStatement -> 
        if env.in_loop then
          env, S.ContinueStatement
        else failwith "Invalid continue statement outside a loop"

      | A.EmptyStatement -> 
        env, S.EmptyStatement

      | _ -> failwith "INTERNAL unhandled statement"
    in 
    let env_new, s_rest = gen_sast env_new rest in
    (env_new, (s_stmt :: s_rest))
