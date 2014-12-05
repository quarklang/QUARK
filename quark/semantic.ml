module A = Ast
module S = Sast
module T = Type

exception Error of string

(* TODO need a valid variable_decl type! may have one already *)
type variable_decl = {
    todo: string
}

type symbol_table = {
    parent: symbol_table option;
    variables: variable_decl list; (* TODO need actual type. see above *)
}

type translation_environment = {
    scope: symbol_table;        (* symbol table for vars (includes funcs) *)
    return_type: Type.vartype;  (* function's return type *)
    return_seen: bool
}

let rec find_variable (scope : symbol_table) name =
    try
      List.find (fun (s, _, _, _) -> s = name) scope.variables
    with Not_found ->
      match scope.parent with
          Some(parent) -> find_variable parent name
        | _ -> raise Not_found

(*extracts the type from a datatype declaration*)
let rec get_type_from_datatype = function
  A.Datatype(typ) -> typ
| A.Arraytype(typ) -> get_type_from_datatype typ

(*extracts the type and name from a Formal declaration*)
let get_name_type_from_formal env = function
    Formal(datatype,ident) -> (ident,datatype,None)

(* Find the variable. If you find the variable:
	Create a new list with the updated variable *)
let update_variable env (name, datatype, value) = 
	let ((_,_,_), location) = 
	try (fun var_scope -> ((List.find (fun (s,_,_) -> s=name) var_scope),1)) env.var_scope.variables
		with Not_found -> try (fun var_scope -> ((List.find (fun (s,_,_) -> s=name) var_scope),2)) env.global_scope.variables
			with Not_found -> raise Not_found in
	let new_envf =
	match location with 
		1 -> 
			(* update local vars *)
			let new_vars = List.map (fun (n, t, v) -> if(n=name) then (name, datatype, value) else (n, t, v)) env.var_scope.variables in
			let new_sym_table = {parent = env.var_scope.parent; variables = new_vars;} in
			let new_env = {env with var_scope = new_sym_table} in
			new_env
		| 2 -> 
			(* update global vars *)
			let new_vars = List.map (fun (n, t, v) -> if(n=name) then (name, datatype, value) else (n, t, v)) env.global_scope.variables in
			let new_sym_table = {parent = env.var_scope.parent; variables = new_vars;} in
			let new_env = {env with global_scope = new_sym_table} in
			new_env
        | _ -> raise(Error("Undefined scope"))
	in new_envf

let update_list expr_list index expr = 
	let xarr = Array.of_list expr_list in
	let _ = Array.set xarr index expr in
	let xlist = Array.to_list xarr in
	xlist

let get_int_from_var env v = 
    let (_,ty,value) = try find_variable env v with Not_found -> raise(Error("Cannot
    index a non-initialized variable")) in match value with
        Some(ExprVal(IntLit(x))) -> x
        | _ -> raise(Error("Non-integer variable value"))


let get_type = function
    | S.Binop(_, _, _, t) -> t
    | S.AssignOp(_, _, _, t) -> t
    | S.Unop(_, _, t) -> t
    | S.PostOp(_, _, t) -> t
    | S.PostOp(_, _, t) -> t
    | S.Assign(_, _, t) -> t
    | S.IntLit(_, t) -> t
    | S.BoolLit(_, t) -> t
    | S.FractionLit(_, _, t) -> t
    | S.QRegLit(_, _, t) -> t
    | S.FloatLit(_, t) -> t
    | S.ComplexLit(_, _, t) -> t
    | S.StringLit(_, t) -> t
    | S.ArrayLit(_, t) -> t
    | S.MatrixLit(_, t) -> t
    | S.Cast(_, _, t) -> t
    | _ -> raise(Error "There is not get_type() implemented for this type");
    (* TODO why don't these have types in sast?
    |FunctionCall(_, _, _, t) -> t
    | Lva(_,
    *)

(*Semantic checking on expressions*)
let rec check_expr expr env =

    (* check_expr() helper functions *)

    let check_array exprs env = 
        (* evaluates each expr in an arr and ensures all exprs are the same type.
         * returns: array of checked exprs * T.vartype of array * env. *)
        let checked_array, array_type, env = List.fold_left

            (* evaluate each expression in the list *)
            (fun (checked_exprs_acc, prev_type, env) unchecked_expr ->
                let checked_expr, env = check_expr unchecked_expr env in
                let expr_type = get_type checked_expr in
                match prev_type with
                    | T.Void ->
                        (* means we're seeing the 1st expr in the array and now know array type *)
                        checked_expr :: checked_exprs_acc, expr_type, env
                    | array_type ->
                        (* ensure all elems in array are the same type *)
                        if array_type <> expr_type
                        then raise(Error "All elements in an array must be the same type");
                        checked_expr :: checked_exprs_acc, array_type, env)

        ([], T.Void, env) exprs in
        List.rev(checked_array), array_type, env
    in

    match expr with
        | A.IntLit(i) -> S.IntLit(i, A.DataType(T.Int)), env
        | A.BoolLit(b) -> S.BoolLit(b, A.DataType(T.Bool)), env
        | A.FloatLit(f) -> S.FloatLit(f, A.DataType(T.Float)), env
        | A.StringLit(s) -> S.StringLit(s, A.DataType(T.String)), env

        | A.FractionLit(num_expr, denom_expr) -> 
            (* checks fraction expressions first *)
            let num_expr, env   = check_expr num_expr env in
            let denom_expr, env = check_expr denom_expr env in
            S.FractionLit(num_expr, denom_expr, A.DataType(T.Fraction)), env

        | A.QRegLit(expr1, expr2) -> 
            (* checks expressions first *)
            let expr1, env   = check_expr expr1 env in
            let expr2, env = check_expr expr2 env in
            S.QRegLit(expr1, expr2, A.DataType(T.QReg)), env

        | A.ComplexLit(real_expr, im_expr) -> 
            (* checks expressions first *)
            let real_expr, env   = check_expr real_expr env in
            let im_expr, env = check_expr im_expr env in
            S.ComplexLit(real_expr, im_expr, A.DataType(T.Complex)), env

        | A.ArrayLit(exprs) ->
            let arr, typ, env = check_array exprs env in
            S.ArrayLit(arr, A.DataType(typ)), env

        | A.MatrixLit(exprs_list_list) ->
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

        | A.Binop(expr1, operation, expr2) -> 
            (* helper functions for Binop case *)
            let logic_relational type1 type2 = match type1, type2 with
                | T.Int,   T.Int 
                | T.Float, T.Float 
                | T.Int,   T.Float 
                | T.Float, T.Int -> T.Bool
                | _ -> raise(Error "Incompatible types for relational logic.") in

            let math type1 type2 = match type1, type2 with
                | T.Float, T.Int
                | T.Int,   T.Float 
                | T.Float, T.Float  -> T.Float
                | T.Int,   T.Int    -> T.Int
                | _ -> raise(Error "Incompatible types for math.") in

            let logic_basic type1 type2 = match type1, type2 with
                | T.Bool, T.Bool -> T.Bool
                | _ -> raise(Error "Incompatible types for basic logic (ie. 'and', 'or').") in

            let logic_equal type1 type2 = match type1, type2 with
                | type1', type2' when type1' == type2' -> T.Bool
                | _ -> raise(Error "Incompatible types for equal logic.") in

            (* check left and right children *)
            let expr1, type1 = check_expr expr1 env
            and expr2, type2 = check_expr expr2 env in

            let result_type = match operation with 
                | A.Add         -> math type1 type2
                | A.Sub         -> math type1 type2
                | A.Mul         -> math type1 type2
                | A.Div         -> math type1 type2
                | A.Mod         -> math type1 type2
                | A.Pow         -> math type1 type2
                | A.Eq          -> logic_equal type1 type2 
                | A.NotEq       -> logic_equal type1 type2
                | A.Less        -> logic_relational type1 type2 
                | A.LessEq      -> logic_relational type1 type2
                | A.Greater     -> logic_relational type1 type2
                | A.GreaterEq   -> logic_relational type1 type2
                | A.And         -> logic_basic type1 type2
                | A.Or          -> logic_basic type1 type2
                (* TODO
                | BitAnd
                | BitOr
                | BitXor
                | Lshift
                | Rshift
                | AddEq
                | SubEq
                | MulEq
                | DivEq
                | AndEq
                | Query
                | QueryUnreal
                *)
                in
            S.Binop(expr1, operation, expr2, A.DataType(result_type)), env

        (*
        | A.AssignOp(lvalue, binop, expr) ->
        | A.Assign(lvalue, expr) ->
        | A.Unop(unop, expr) ->
            (* TODO
             * is lvalue variable in table
             * is lvalue of type int?
             *)
        | A.PostOp(lvalue, postop) ->
            (* TODO
             * is lvalue variable in table
             * is lvalue of type int?
             *)
            S.Datatype(T.Int), T.Int

        | Variable(v) -> 
            let (_,s_type,_) = try find_variable env v with 
                Not_found ->
                    raise (Error("Undeclared Identifier " )) in s_type
        | Unop(u, e) -> 
            let t = check_expr e env in 
            (match u with
                Not -> if t = Datatype(Boolean) then t else raise (Error("Cannot negate a non-boolean value"))
                | _ -> if t = Datatype(Int) then t else if t = Datatype(Float) then t 
                            else
                                raise (Error("Cannot perform operation on " )))
        | ArrElem(id, expr) -> 
            (* return SArrElem(id, expr, datatype) where:
                id is name of array variable
                expr has datatype Int
                datatype is type of array *)
            let (_, ty, v) = try find_variable env id with
                Not_found -> raise(Error("Uninitialized array")) in
            let el_type = (match ty with 
                Arraytype(Datatype(x)) -> Datatype(x)
                | _ -> raise(Error("Cannot index a non-array expression"))) in
            let expr_type = check_expr expr env in 
            let sty = match expr_type with
                Datatype(ty) -> Some(ty) 
                | Arraytype(dt) -> None in
            let ty = match sty with
                Some(ty) -> ty
                | None -> raise(Error("Can't invoke array element as index")) in
            let _ = if not(ty=Int) then raise(Error("index must be an integer")) in
            (el_type)
        | ExprAssign(id, e) -> let (_,t1,_) = (find_variable env id) and t2 =
            check_expr e env
            in (if not (t1 = t2) then (raise (Error("Mismatch in types for assignment")))); check_expr e env
        | Cast(ty, e) -> ty
        | Call(Ident("print"),e) -> let _ = List.map(fun exp -> check_expr exp env) e in
                    Datatype(Void)
        | Call(Ident("print_time"),e) -> let _ = List.map(fun exp -> check_expr exp env) e in
                    Datatype(Void)
        | Call(id, e) -> try (let (fname, fret, fargs, fbody)  = find_function env.fun_scope id in
                    let el_tys = List.map (fun exp -> check_expr exp env) e in
                    let fn_tys = List.map (fun farg-> let (_,ty,_) = get_name_type_from_formal env farg in ty) fargs in
                    if not (el_tys = fn_tys) then
                        raise (Error("Mismatching types in function call")) else
                        Datatype(fret))
                with Not_found ->
                    raise (Error("Undeclared Function "))
    *)

let get_val_type env = function
    ExprVal(expr) -> check_expr expr env
    | ArrVal(expr_list) -> check_expr (List.hd expr_list) env


let get_var_scope env name =  
    try (let (_,_,_) = List.find (fun (s,_,_) -> s=name) env.var_scope.variables in Local)
          with Not_found -> try (let (_,_,_) = List.find(fun (s,_,_) -> s=name) env.global_scope.variables in Global)
                with Not_found -> raise(Error("get_var_scope is failing"))
                
(*converts expr to sexpr*)
let rec get_sexpr env e = match e with
      IntLit(i) -> SIntLit(i, Datatype(Int))
      | BoolLit(b) -> SBoolLit(b,Datatype(Boolean))
      | FloatLit(f) -> SFloatLit(f,Datatype(Float))
      | StringLit(s) -> SStringLit(s,Datatype(String))
      | Variable(id) -> SVariable(SIdent(id, get_var_scope env id), check_expr e env)
      | Unop(u,ex) -> SUnop(u, get_sexpr env ex, check_expr e env)
      | Binop(e1,b,e2) -> SBinop(get_sexpr env e1,b, get_sexpr env e2,check_expr e env)
      | ArrElem(id, expr) -> SArrElem(SIdent(id, get_var_scope env id), get_sexpr env expr, check_expr env expr)
      | ExprAssign(id,ex) -> SExprAssign(SIdent(id, get_var_scope env id),
      get_sexpr env ex,check_expr e env) 
      | Cast(ty,ex) -> SCast(ty,get_sexpr env ex,ty)
	  | Call(Ident("print"),ex_list) -> let s_ex_list = List.map(fun exp -> get_sexpr env exp) ex_list 
	  in SCall(SIdent(Ident("print"),Global),s_ex_list,check_expr e env)
	  | Call(Ident("print_time"),ex_list) -> let s_ex_list = List.map(fun exp -> get_sexpr env exp) ex_list
	  in SCall(SIdent(Ident("print_time"),Global), s_ex_list, check_expr e env)
      | Call(id, ex_list) -> let s_ex_list = List.map(fun exp -> get_sexpr env
      exp) ex_list in SCall(SIdent(id,Global),s_ex_list, check_expr e env) 

(* Make sure a list contains all items of only a single type; returns (sexpr list, type in list) *)
let get_sexpr_list env expr_list = 
	let sexpr_list = 
		List.map (fun expr -> 
				let t1 = get_type_from_datatype(check_expr (List.hd expr_list) env) 
				and t2 = get_type_from_datatype (check_expr expr env) in
				if(t1=t2) then get_sexpr env expr 
					else raise (Error("Type Mismatch"))
				 ) expr_list in sexpr_list


(* replacement for get_typed_value *)
let get_sval env = function
		ExprVal(expr) -> SExprVal(get_sexpr env expr)
		| ArrVal(expr_list) -> SArrVal(get_sexpr_list env expr_list)

let get_datatype_of_list env expr_list = 
	let ty = List.fold_left (fun dt1 expr2 -> 
								let dt2 = check_expr expr2 env in
								if(dt1 = dt2) then dt1 else raise (Error("Inconsistent array types"))) (check_expr (List.hd expr_list) env) expr_list in ty


let get_datatype_from_val env = function
	ExprVal(expr) -> check_expr expr env
	| ArrVal(expr_list) -> get_datatype_of_list env expr_list

(* if variable is not found, then add it to table and return SVarDecl *)
(* if variable is found, throw an error: multiple declarations *)
let get_sdecl env decl = match decl with
(* if ident is in env, return typed sdecl *)
  A.PrimitiveDecl(datatype, ident) -> S.PrimitiveDecl(datatype, S.Ident(ident))
| A.AssigningDecl(datatype, ident, expression) ->
    let expr_val = get_sval env expression in
    (S.AssigningDecl(datatype, S.Ident(ident), expr_val), env)

let get_name_type_from_decl decl = match decl with
  A.PrimitiveDecl(datatype, ident) -> (ident, datatype)
| A.AssigningDecl(datatype,ident,value) -> (ident, datatype)

let get_name_type_val_from_decl decl = match decl with
  A.PrimitiveDecl(datatype, ident) -> (ident, datatype, None)
| A.AssigningDecl(datatype, ident, value) -> (ident, datatype, Some(value))

(* returns tuple (left hand id, left hand id type, right hand value type) *)
let get_name_type_from_var env = function
  A.PrimitiveDecl(datatype,ident) -> (ident,datatype,None)
| A.AssigningDecl(datatype,ident,value) -> (ident,datatype,Some(value))

(* function to check if a range is valid for a range iterator *)
let check_range range env = function
  A.Range(expr1, expr2, expr3) ->
    let expr1, typ1 = check_expr expr1 env in
    let expr2, typ2 = check_expr expr2 env in
    let expr3, typ3 = check_expr expr3 env in
    if not (typ1 = FloatLit || typ1 = IntLit) ||
       not (typ2 = FloatLit || typ2 = IntLit) ||
       not (typ3 = FloatLit || typ3 = IntLit) then
        raise (Error("Improper Array Iterator in For statement"));
    else S.Range(expr1, expr2, expr3)

(*function that adds variables to environment's var_scope for use in functions*)
let add_to_var_table env name t v = 
	let new_vars = (name,t, v)::env.var_scope.variables in
	let new_sym_table = {parent = env.var_scope.parent; variables = new_vars;} in
	let new_env = {env with var_scope = new_sym_table} in
	new_env

(*function that adds variables to environment's global_scope for use with main*)
let add_to_global_table env name t v = 
	let new_vars = (name,t,v)::env.global_scope.variables in
	let new_sym_table = {parent=env.global_scope.parent; variables = new_vars;} in
	let new_env = {env with global_scope = new_sym_table} in
	new_env

(* check both sides of an assignment are compatible*) 
let check_assignments type1 type2 = match (type1, type2) with
	(Int, Int) -> true
	|(Float, Float) -> true
	|(Int, Float) -> true
	|(Float, Int) -> true
	|(Boolean, Boolean) -> true
	|(String, String) -> true
	|(_,_) -> false

(* checks the type of a variable in the symbol table*)
(* Changed from "check_var_type" *)
let match_var_type env v t =
	let(name,ty,value) = find_variable env v in
	if(t<>ty) then false else true

(* Checks that a function returned if it was supposed to*)
let check_final_env env =
	(if(false = env.return_seen && env.return_type <> Datatype(Void)) then
		raise (Error("Missing Return Statement")));
	true

(* Default Table and Environment Initializations *)
let empty_table_initialization = {
    parent=None;
    variables=[];
}
let empty_function_table_initialization = {
    functions=[(Ident("print_string"), Void, [Formal(Datatype(String), Ident("s"))],[]);(Ident("print_int"),Void,[Formal(Datatype(Int),Ident("s"))],[])]
}
let empty_environment = {
    return_type = Datatype(Void);
    return_seen = false;
    location="main";
    global_scope = empty_table_initialization;
    var_scope = empty_table_initialization;
    fun_scope = empty_function_table_initialization
}

(* TODO remove. leave now for references
let initialize_globals (globals, env) decl = 
	let (name, ty) = get_name_type_from_decl decl in
		let ((_,dt,_),found) = try (fun f -> ((f env name),true)) find_global_variable with 
			Not_found ->
				((name,ty,None),false) in
		let ret = if(found=false) then
			match decl with
				VarDecl(datatype,ident) ->
		        	let (name,ty,_) = get_name_type_from_var env decl in
		            let new_env = add_to_global_table env name ty None in
		            (SVarDecl(datatype,SIdent(ident,Global))::globals, new_env)
				| VarAssignDecl(dt, id, value) ->
					let t1 = get_type_from_datatype(dt) and t2 = get_type_from_datatype(get_datatype_from_val env value) in
					if(t1=t2) then
						let (n, t, v) = get_name_type_val_from_decl decl in
						let new_env = add_to_global_table env n t v in
						(SVarAssignDecl(dt,SIdent(id,Global),get_sval env value)::globals, new_env)
					else raise (Error("Type mismatch"))
				else
					raise (Error("Multiple declarations")) in ret
*)

(* semantic checking on a stmt*)
let rec check_stmt stmt env = match stmt with
    | A.CompoundStatement(stmts) ->
        (* creates; new scope: parent is the existing scope, start out empty *)
        let scope' = { parent = Some(env.scope); variables = [] } in

        (* creates new env: same, but with a fresh symbol table *)
        let env = { env with scope = scope'; } in

        (* recursively check each statement in the block *)
        let statements, env = check_stmts stmts env in

        (* TODO not sure if we need scope'.variables <- List.rev scope'.variables; microc does this. *)
        (* TODO where dafuq do we pop the scope after a block? *)

        S.CompoundStatement(statements), env

    | A.Expression(expr) -> 
        let expr, typ = check_expr expr env in
        S.Expression(expr), env

    | A.ReturnStatement(expr) ->
        let expr, typ = check_expr expr env in
        if t <> env.return_type then
          raise Error("Incorrect return type");
        let env = { env with return_seen = true } in
        S.ReturnStatement(expr), env

    | A.IfStatement(expr, stmt1, stmt2) ->
        let expr = get_type_from_datatype(check_expr expr env) in
        if not expr = T.Boolean then
          raise Error("Predicate of an if statement must be boolean");
        
        let (st1, env1) =check_stmt s1 env
        and (st2, env2) = check_stmt s2 env in
        let this_return_seen = (env1.return_seen && env2.return_seen) in
        let env = { env with return_seen = this_return_seen } in
        S.IfStatement((get_sexpr env e), s1, s2), env

    (* CHUNK 2 medium. 
        INTERFACE
        NOTE: currently our sast has 'iterator list' like vector. we probably 
                need to change that to just iterator. so do that too.
        
        you need to match on iterator, ie. | A.ForStatement(iter, statement)
        - ensure iter is valid iterator. not entirely sure how to do this but
          there are only two types of iterators so just case match on those two
          and throw an error if not successful
        - while checking ArrayIterator you will need to recursively evaluate
          check_expr because there might be side effects
        - return S.WhileStatement(check_stmt statement env) which will recursively
          validate statement

        - Need to check if iter is A.RangeIterator or A.ArrayIterator
        - if RangeIterator, need to check RangeIterator(ident, range)
            - need to make sure ident is defined, and that range is a valid
            - range(e1, e2, e3) -> validate all three expressions; 
            - make sure e1, e2, and e3 are all IntLits and e1 < e2
        - if ArrayIterator, need to check ArrayIterator(ident, expr)
            - ident is 'x' in 'x in arr'; need to put ident into environment
              make sure expr is a valid ArrayElem

        - need to write check_range function
        - need to write get_siterator function
        - need to write get_srange function*)

    | A.ForStatement(iter, stmt) ->
        match iter with
            | A.ArrayIterator(ident, _expr) -> 
                let expr, typ = check_expr _expr env in 
                if not (typ = ArrayLit) then
                    raise (Error("Improper Array Iterator in For statement"));  
                let semantic_stmt, env = check_stmt stmt env in   
                S.ForStatement(S.ArrayIterator(ident, expr), semantic_stmt), env
            | A.RangeIterator(ident, range) ->
                let semantic_range = check_range range env in
                if not (typ = ArrayLit) then
                    raise (Error("Improper Range Iterator in Ffor statement"));                
                let semantic_stmt, env = check_stmt stmt env in  
                S.ForStatement(S.RangeIterator(ident, semantic_range), semantic_stmt), env

    (* CHUNK 3 easy *)
    (* INTERFACE
        - verify expression is valid by running check_expr expression env
        - pass statement back into check_stmt() function. *)
    | A.WhileStatement(expr, stmt) ->
        let expr, typ = get_type_from_datatype(check_expr e env) in
        if not(typ = Boolean) then
            raise (Error("Improper While loop format"));
        let semantic_stmt, env = check_stmt stmt env in
        S.WhileStatement(S.Expression(expr), semantic_stmt)

    (* CHUNK 4 hard *)
    (* INTERFACE 
        match cases for each type of declaration, ie. 
        | A.AssigningDecl ->
        | A.PrimitiveDecl ->
        | A.ArrayDecl ->
        
        here is the gist of what each of these should do:
        - add to *or* modify the scope. check if variable with the same 
          name exists, if it does change scope, else add new var to scope
        - call check_expr to evaluate right side of decl
        - ensure datatype and check_expr match
        *)
    | A.Declaration(decl) -> 

        (* If variable is found, throw multiple declarations error and if
         * variable not found and var is AssignDecl, make sure types are compatible *)
        let (name, datatype) = get_name_type_from_decl decl in
          try
            let (_,datatype,_) = find_variable env name in (ident, datatype, true)
          with Not_found -> ((name,ty,None),false) in
            let ret = if(found=false) then
              match decl with
                  PrimitiveDecl(_,_) ->
                    let (sdecl,_) = get_sdecl env decl in
                    let (n, t, v) = get_name_type_val_from_decl decl in
                    let new_env = add_to_var_table env n t v in
                    (SDeclaration(sdecl), new_env)
                | AssigningDecl(datatype, ident, expression) ->
                    let t1 = get_type_from_datatype(dt) and t2 = get_type_from_datatype(get_datatype_from_val env value) in
                    if(t1=t2) then
                        let (sdecl,_) = get_sdecl env decl in
                        let (n, t, v) = get_name_type_val_from_decl decl in
                        let new_env = add_to_var_table env n t v in
                        (SDeclaration(sdecl), new_env)
                    else raise (Error("Type mismatch"))
                else
                    raise (Error("Multiple declarations")) in ret

    (* CHUNK 5 hard
    | A.FunctionDecl case
    | A.ForwardDecl case
        - ideally we would move FunctionDecl and ForwardDecl to be decl's in both Sast and Ast
          this *shouldn't* cause any shift reduce errors and it would help to get 
          that done so please make it a decl and ensure no shift reduce errors
        - store new or modify existing function in env.scope
        - recursively validate and store all parameters in 'decl list' by calling
          something like List.map (fun statement -> check_stmt statement env) decl_list
        - validate each statement in statement_list and ensure last type 
          is ReturnType with matching type as the defined return type
          NOTE: I believe this is done by modifying the env.scope.return_type. 
                you sets the return_type and then that is checked in the 
                S.ReturnType case in check_stmt(). What you need to do is make sure that
                the last statement in statement_list is of type ReturnType
    *)

    (* CHUNK 6 medium
        - move this to check_expr() since we have it defined as an expr
        - pattern match on lvalue parameter: A.Variable and A.ArrayElem
        - do we need to add MatrixElem to type lvavlue in (S)Ast? Maybe ask to Jim
          add it if necessary and handle that case as well.
        - for each case, store/modify env.scope
        - ensure types don't conflict (probably by recursively calling check_expr()
    | A.Assign(ident, expr) ->
        (* make sure 1) variable exists, 2) variable and expr have same types *)
        let (_, dt, _) = try find_variable env ident with Not_found -> raise (Error("Uninitialized variable")) in
        let t1 = get_type_from_datatype dt 
        and t2 = get_type_from_datatype(check_expr expr env) in
        if( not(t1=t2) ) then 
            raise (Error("Mismatched type assignments"));
        let sexpr = get_sexpr env expr in
        let new_env = update_variable env (ident,dt, Some((Expression(expr)))) in
        S.Assign(S.Ident(ident, get_var_scope env ident), sexpr)

    | A.ArrAssign(ident, expr_list) ->
        (* make sure 1) array exists and 2) all types in expr list are equal *)
        let (n,dt,v) = try find_variable env ident with Not_found -> raise (Error("Undeclared array")) in
        let sexpr_list = List.map (fun expr2 -> 
                            let expr1 = List.hd expr_list in
                            let t1 = get_type_from_datatype(check_expr expr1 env) and t2 = get_type_from_datatype(check_expr expr2 env) in
                            if(t1=t2) then 
                                let sexpr2 = get_sexpr env expr2 in sexpr2
                                else raise (Error("Array has inconsistent types"))) expr_list in
        let _ = 
            let t1=get_type_from_datatype(check_expr (List.hd expr_list) env) and t2=get_type_from_datatype(dt) in
            if(t1!=t2) then raise (Error("Type Mismatch")) in
        let new_env = update_variable env (n,dt,(Some(A.ArrayLit(expr_list)))) in
        S.ArrAssign(SIdent(ident,get_var_scope env ident), sexpr_list)

    | A.ArrElemAssign(ident, expr1, expr2) ->
        (* Make sure
            1) array exists (if it exists, then it was already declared and semantically checked)
            2) expr matches type of array 
            3) index is not out of bounds *)
        let (id, dt, v) = try find_variable env ident with Not_found -> raise (Error("Undeclared array")) in
        let t1 = get_type_from_datatype(dt) and t2 = get_type_from_datatype(check_expr expr2 env) in
        let _ = if(t1=t2) then true else raise (Error("Type Mismatch")) in
        let _ = (match v with
                Some(ArrVal(el)) -> (* get_sexpr_list env  *)el
                | None -> raise (Error("No expression on right hand side"))
                | _ -> raise (Error("???"))) in
        let t = get_type_from_datatype(check_expr expr1 env) in
        let _ = if not(t=Int) then raise(Error("Array index must be an integer")) in
        S.ArrElemAssign(SIdent(ident,get_var_scope env ident), get_sexpr env expr1, get_sexpr env expr2)
    *)

    (* CHUNK 7 easy? 
    | A.VoidReturnStatement
    - might just be `| A.VoidReturnStatement() -> S.VoidReturnStatement()`
    *)

    (* CHUNK 8 easy. do we need? probably not 
    | Terminate -> (STerminate, env)
     * *)

(* runs each stmt in a stmt-list through check_stmt() *)
let check_stmts stmts env =
    let checked_stmts, env = List.fold_left 
        (fun (checked_stmt_list, curr_env) unchecked_stmt -> 
            let checked_stmt, new_env = check_stmt(unchecked_stmt curr_env) in
            checked_stmt :: checked_stmt_list, new_env)
    ([], env) stmts in
    (* rev list because of fold_left *)
    let checked_stmts = List.rev checked_stmts in
    checked_stmts, env

(* Takes a range and checks if expressions return Ints 
   Returns semantically checked expressions 1, 2, 3 and a Boolean *)
let rec check_range env range =
    match range with
    Range(e1, e2, e3) ->
        let (se1, t1) = check_expr e1 env in
        let (se2, t2) = check_expr e2 env in 
        let (se3, t3) = check_expr e3 env in 
        if not (t1 = T.Int && t2 = T.Int && t3 = T.Int) then 
        raise (Error("Range only accepts integer values for start, stop, and step")) in 
        S.Range(se1, se2, se3)

let get_sstmt_list env stmt_list = 
    List.fold_left
        (fun (sstmt_list,env) stmt ->
            let (sstmt, new_env) = check_stmt stmt env in 
            (sstmt::sstmt_list, new_env))
    ([],env) stmt_list

    (* add a function to the environment*)
    let add_function env sfunc_decl =
    let f_table = env.fun_scope in
    let old_functions = f_table.functions in
    match sfunc_decl with
        SFunc_Decl(sfuncstr, datatype) ->
            let func_name = sfuncstr.sfname in
            let func_type = get_type_from_datatype sfuncstr.sreturn in
            let func_formals = sfuncstr.sformals in
            let func_body = sfuncstr.sbody in
            let new_functions = (func_name, func_type, func_formals, func_body)::old_functions in
            let new_fun_scope = {functions = new_functions} in
            let final_env = {env with fun_scope = new_fun_scope} in
            final_env

(* Semantic checking on a function*)
(* TODO remove. checking functions should be in either check_stmt() or check_expr()
let check_func env func_declaration =
    let new_locals = List.fold_left
        (fun a vs -> (get_name_type_from_formal env vs)::a)
    [] func_declaration.formals in

    let new_var_scope = {
        parent=Some(env.var_scope);
        variables = new_locals;
    } in
    let new_env = {
        return_type = func_declaration.return;
        return_seen=false;
        location="in_func";
        global_scope = env.global_scope;
        var_scope = new_var_scope;
        fun_scope = env.fun_scope
    } in
    let (typed_statements, final_env) = get_sstmt_list new_env func_declaration.body in
    let _ = check_final_env final_env in
    let sfuncdecl = ({
        sreturn = func_declaration.return;
        sfname = func_declaration.fname;
        sformals = func_declaration.formals;
        sbody = typed_statements
    }) in
    (SFunc_Decl(sfuncdecl,func_declaration.return), env) 
*)

(* TODO remove below but keep as reference for the moment
let initialize_functions env function_list = 
    let (typed_functions, last_env) = List.fold_left
        (fun (sfuncdecl_list, env) func ->
            let (sfuncdecl, _) = check_func env func in
            let final_env = add_function env sfuncdecl in
            (sfuncdecl::sfuncdecl_list, final_env))
        ([],env) function_list in
        (typed_functions, last_env)
*)


(* checks the semantics of a program *)
let check_program ast =
    (* creates new scope and env *)
    let scope' = { parent=None; variables=[]; } in
    let env = { scope: scope'; return_type: T.Void; } in
    (* TODO should the toplevel return_type be int or bool? *)
    (* check all toplevel stmts *)
    let stmts, env = check_stmts ast env in stmts

    (* TODO remove below but keep as reference for the moment
    let (functions,(globals, threads)) = ast in
    let env = empty_environment in
    let (typed_functions, new_env) = initialize_functions env functions in
    let (typed_globals, new_env2) = List.fold_left(
        fun (new_globals, env) globals -> 
            initialize_globals (new_globals, env) globals) 
        ([], new_env) globals in
    Prog(typed_functions, (typed_globals, typed_threads))
    *)
