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
    let _ = print_endline @@ "DEBUG ARRAY " ^ (A.str_of_datatype _tmp) in
    env, S.ArrayLit(elem_type, s_exprs), A.ArrayType(elem_type)

  | A.MatrixLit(exprs_list_list) ->
    let env, s_matrix, elem_type, coldim = gen_s_matrix env exprs_list_list in
    let _tmp = A.MatrixType(A.DataType(elem_type)) in
    let _ = print_endline @@ "DEBUG MATRIX " ^ (A.str_of_datatype _tmp) 
        ^ " cols= " ^ string_of_int coldim ^ " rows= " ^ string_of_int (List.length exprs_list_list) 
    in
    env, S.MatrixLit(elem_type, s_matrix, coldim), A.MatrixType(A.DataType(elem_type))
  
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
      let _ = print_endline @@ "DEBUG ASSIGN returns "^A.str_of_datatype return_type in
      env, S.Assign(s_lval, s_rhs_ex), return_type

  (* Function calls *)
  | A.FunctionCall(func_id, ex_list) -> 
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
            let _, s_ex, ex_type = gen_s_expr env ex in 
            match ex_type, f_arg with
            | A.DataType(T.Int), A.DataType(T.Float)
            | A.DataType(T.Float), A.DataType(T.Int) -> s_ex
            | ex_type', f_arg' when ex_type' = f_arg' -> s_ex
            | _ -> failwith @@ "Incompatible args for function " ^fidstr^ ": "
                  ^ A.str_of_datatype ex_type ^ " given but " 
                  ^ A.str_of_datatype f_arg ^ " expected"   
          ) ex_list f_args
        in
        env, S.FunctionCall(fidstr, s_ex_list), finfo.f_return
      else
        failwith @@ "Function " ^fidstr^ " requires " ^ string_of_int farg_len
            ^ " arg but " ^ string_of_int actual_len ^ " provided"
    else
      failwith @@ if finfo.f_return = A.NoneType then
        "Function " ^ fidstr ^ " is undefined" else
        "Only forward declaration, no actual definition is found for " ^ fidstr
  
  (* Membership testing with keyword 'in' *)
  | A.Membership(elem, array) -> 
    failwith "Membership not yet supported"
    (* !!!! Needs to assign exElem and exArray to compiled temp vars *)
    (*
    let exElem = gen_s_expr exElem in
    let exArray = gen_s_expr exArray in
      "std::find(" ^surr exArray^ ".begin(), " ^surr exArray^ ".end(), " ^
      exElem^ ") != " ^surr exArray^ ".end()"
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
    (env, [], T.Void, 0) exprs_list_list in
  (env, List.rev matrix , matrix_type, row_length)
  
(*
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
        let env' = incr_env_depth env in
        let env' = update_env_func env' return_type func_id param_list true in
        let _ = debug_env env' "after FunctionDecl" in
        (* get the function declaration, then close 'func_current' *)
        let function_decl = S.FunctionDecl(return_type, get_id func_id, s_param_list, 
          snd_2 @@ gen_sast env' stmt_list) in
        let env'' = { 
          var_table = env.var_table; 
          func_table = env'.func_table;
          func_current = "";
          depth = env.depth;
        } in
        let _ = debug_env env'' "closed after FuncDecl" in
        env'', function_decl
      
      | A.ForwardDecl(return_type, func_id, param_list) -> 
        let s_param_list = gen_s_param_list param_list in
        let env' = update_env_func env return_type func_id param_list false in
        env', S.ForwardDecl(return_type, get_id func_id, s_param_list)

      (* statements *)
      | A.IfStatement(pred_ex, stmt_if, stmt_else) -> 
        let env, s_pred_ex, pred_type = gen_s_expr env pred_ex in
        if pred_type = A.DataType(T.Bool) then
          let env, s_stmt_if = gen_sast env [stmt_if] in
          let env, s_stmt_else = gen_sast env [stmt_else] in
          env, S.IfStatement(s_pred_ex, List.hd s_stmt_if, List.hd s_stmt_else)
        else
          failwith @@ "If predicate must be bool, but " 
              ^ A.str_of_datatype pred_type ^ " provided"
				
      | A.WhileStatement(pred_ex, stmt) -> 
        let env, s_pred_ex, pred_type = gen_s_expr env pred_ex in
        if pred_type = A.DataType(T.Bool) then
          let env, s_stmt = gen_sast env [stmt] in
          env, S.WhileStatement(s_pred_ex, List.hd s_stmt)
        else
          failwith @@ "While predicate must be bool, but " 
              ^ A.str_of_datatype pred_type ^ " provided"
            
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
        env', S.Declaration(s_dec)

      | A.Expression(ex) -> 
        let env', s_ex, _ = gen_s_expr env ex in
        env', S.Expression(s_ex)

      | A.ReturnStatement(ex) -> 
        (env, S.EmptyStatement)
        (* print_endline @@ "return " ^ gen_s_expr ex ^ ";" *)

      | A.EmptyStatement -> 
        env, S.EmptyStatement

      | A.VoidReturnStatement -> 
        env, S.EmptyStatement
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
