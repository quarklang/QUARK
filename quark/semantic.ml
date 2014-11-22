open Ast
open Sast
open Type

exception Error of string

(*a symbol table consisting of the parent as the variables*)
type symbol_table = {
	parent: symbol_table option;
	(* Added value so that we can check "out of bounds" error on arrays *)
	variables: (ident * datatype * value option) list;
	(* Arrays are expliclty added here with their*)
	(* arrays: (ident * datatype * sexpr list) list *)
}

(*a function table containing function definitions*)
type function_table = {
	functions: (ident * var_type * formal list * sstmt list) list
}

(*our environment*)
type translation_environment = {
	return_type: datatype;	(*function's return type*)
	return_seen: bool;		(*does the function have a return statement*)
	location: string;		(*init, always, main, or function name, used for global or local checking*)
	global_scope: symbol_table;	(*symbol table for global vairables*)
	var_scope: symbol_table; 	(*symbol table for local variables*)
	fun_scope: function_table;	(*symbol table for functions*)
}

(* search for a function in our function table*)
let rec find_function (fun_scope: function_table) name = 
	List.find (fun (s,_,_,_) -> s=name) fun_scope.functions

let basic_math t1 t2 = match (t1, t2) with
	(Float, Int) -> (Float, true)
	| (Int, Float) -> (Float, true)
	| (Int, Int) -> (Int, true)
	| (Float, Float) -> (Int, true)
	| (_,_) -> (Int, false)

let relational_logic t1 t2 = match (t1, t2) with
    (Int,Int) -> (Boolean,true)
    | (Float,Float) -> (Boolean,true)
    | (Int,Float) -> (Boolean,true)
    | (Float,Int) -> (Boolean,true)
	| (_,_) -> (Boolean, false) 

let basic_logic t1 t2 = match(t1,t2) with
    (Boolean,Boolean) -> (Boolean,true)
    | (_,_) -> (Int,false)

let equal_logic t1 t2 = match(t1,t2) with
    (Boolean,Boolean) -> (Boolean,true)
    | (Int,Int) -> (Boolean,true)
    | (Float,Float) -> (Boolean,true)
    | (Int,Float) -> (Boolean,true)
    | (Float,Int) -> (Boolean,true)
    | (String,String) -> (Boolean,true)
    | (_,_) -> (Int,false) 

(*extracts the type from a datatype declaration*)
let rec get_type_from_datatype = function
	Datatype(t)->t
	| Arraytype(ty) -> get_type_from_datatype ty

let get_binop_return_value op typ1 typ2 = 
	let t1 = get_type_from_datatype typ1 and t2 = get_type_from_datatype typ2 in
	let (t, valid) = 
		match op with 
			Add -> basic_math t1 t2
			| Sub -> basic_math t1 t2
			| Mult -> basic_math t1 t2
			| Div -> basic_math t1 t2
			| Mod -> basic_math t1 t2
			| Equal -> equal_logic t1 t2 
			| Neq -> equal_logic t1 t2
			| Less -> relational_logic t1 t2 
			| Leq -> relational_logic t1 t2
			| Greater -> relational_logic t1 t2
			| Geq -> relational_logic t1 t2
			| And -> basic_logic t1 t2
			| Or -> basic_logic t1 t2
		in (Datatype(t), valid) 

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

(*search for variable in global and local symbol tables*)
let find_variable env name =
	try List.find (fun (s,_,_) -> s=name) env.var_scope.variables
	with Not_found -> try List.find(fun (s,_,_) -> s=name) env.global_scope.variables
	with Not_found -> raise Not_found

let get_int_from_var env v = 
    let (_,ty,value) = try find_variable env v with Not_found -> raise(Error("Cannot
    index a non-initialized variable")) in match value with
        Some(ExprVal(IntLit(x))) -> x
        | _ -> raise(Error("Non-integer variable value"))

(*Semantic checking on expressions*)
let rec check_expr env e = match e with
    IntLit(i) ->Datatype(Int)
    | BoolLit(b) -> Datatype(Boolean)
    | FloatLit(f) -> Datatype(Float)
    | StringLit(s) -> Datatype(String)
    | Variable(v) -> 
    	let (_,s_type,_) = try find_variable env v with 
    		Not_found ->
            	raise (Error("Undeclared Identifier " )) in s_type
    | Unop(u, e) -> 
    	let t = check_expr env e in 
        (match u with
        	Not -> if t = Datatype(Boolean) then t else raise (Error("Cannot negate a non-boolean value"))
        	| _ -> if t = Datatype(Int) then t else if t = Datatype(Float) then t 
        				else
            				raise (Error("Cannot perform operation on " )))
    | Binop(e1, b, e2) -> 
    	let t1 = check_expr env e1 and t2 = check_expr env e2 in 
    	let (t, valid) = get_binop_return_value b t1 t2 in
    	if valid then t else raise(Error("Incompatible types with binary
        operator"));

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
		let expr_type = check_expr env expr in 
		let sty = match expr_type with
			Datatype(ty) -> Some(ty) 
			| Arraytype(dt) -> None in
		let ty = match sty with
			Some(ty) -> ty
			| None -> raise(Error("Can't invoke array element as index")) in
		let _ = if not(ty=Int) then raise(Error("index must be an integer")) in
		(el_type)
    | ExprAssign(id, e) -> let (_,t1,_) = (find_variable env id) and t2 =
        check_expr env e 
        in (if not (t1 = t2) then (raise (Error("Mismatch in types for assignment")))); check_expr env e
    | Cast(ty, e) -> ty
	| Call(Ident("print"),e) -> let _ = List.map(fun exp -> check_expr env exp) e in
				Datatype(Void)
	| Call(Ident("print_time"),e) -> let _ = List.map(fun exp -> check_expr env exp) e in
				Datatype(Void)
    | Call(id, e) -> try (let (fname, fret, fargs, fbody)  = find_function env.fun_scope id in
                let el_tys = List.map (fun exp -> check_expr env exp) e in
                let fn_tys = List.map (fun farg-> let (_,ty,_) = get_name_type_from_formal env farg in ty) fargs in
                if not (el_tys = fn_tys) then
                    raise (Error("Mismatching types in function call")) else
                    Datatype(fret))
            with Not_found ->
                raise (Error("Undeclared Function "))

let get_val_type env = function
    ExprVal(expr) -> check_expr env expr
    | ArrVal(expr_list) -> check_expr env (List.hd expr_list)


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
      | Variable(id) -> SVariable(SIdent(id, get_var_scope env id), check_expr env e)
      | Unop(u,ex) -> SUnop(u, get_sexpr env ex, check_expr env e)
      | Binop(e1,b,e2) -> SBinop(get_sexpr env e1,b, get_sexpr env e2,check_expr env e)
      | ArrElem(id, expr) -> SArrElem(SIdent(id, get_var_scope env id), get_sexpr env expr, check_expr env expr)
      | ExprAssign(id,ex) -> SExprAssign(SIdent(id, get_var_scope env id),
      get_sexpr env ex,check_expr env e) 
      | Cast(ty,ex) -> SCast(ty,get_sexpr env ex,ty)
	  | Call(Ident("print"),ex_list) -> let s_ex_list = List.map(fun exp -> get_sexpr env exp) ex_list 
	  in SCall(SIdent(Ident("print"),Global),s_ex_list,check_expr env e)
	  | Call(Ident("print_time"),ex_list) -> let s_ex_list = List.map(fun exp -> get_sexpr env exp) ex_list
	  in SCall(SIdent(Ident("print_time"),Global), s_ex_list, check_expr env e)
      | Call(id, ex_list) -> let s_ex_list = List.map(fun exp -> get_sexpr env
      exp) ex_list in SCall(SIdent(id,Global),s_ex_list, check_expr env e) 

(* Make sure a list contains all items of only a single type; returns (sexpr list, type in list) *)
let get_sexpr_list env expr_list = 
	let sexpr_list = 
		List.map (fun expr -> 
				let t1 = get_type_from_datatype(check_expr env (List.hd expr_list)) 
				and t2 = get_type_from_datatype (check_expr env expr) in
				if(t1=t2) then get_sexpr env expr 
					else raise (Error("Type Mismatch"))
				 ) expr_list in sexpr_list


(* replacement for get_typed_value *)
let get_sval env = function
		ExprVal(expr) -> SExprVal(get_sexpr env expr)
		| ArrVal(expr_list) -> SArrVal(get_sexpr_list env expr_list)

let get_datatype_of_list env expr_list = 
	let ty = List.fold_left (fun dt1 expr2 -> 
								let dt2 = check_expr env expr2 in
								if(dt1 = dt2) then dt1 else raise (Error("Inconsistent array types"))) (check_expr env (List.hd expr_list)) expr_list in ty


let get_datatype_from_val env = function
	ExprVal(expr) -> check_expr env expr
	| ArrVal(expr_list) -> get_datatype_of_list env expr_list

(* if variable is not found, then add it to table and return SVarDecl *)
(* if variable is found, throw an error: multiple declarations *)
let get_sdecl env decl = match decl with
	(* if ident is in env, return typed sdecl *)
    VarDecl(datatype, ident) -> (SVarDecl(datatype, SIdent(ident, Local)), env)
	| VarAssignDecl(datatype, ident, value) -> 
		let sv = get_sval env value in
	(SVarAssignDecl(datatype, SIdent(ident, Local), sv), env)

let get_name_type_from_decl decl = match decl with
	VarDecl(datatype, ident) -> (ident, datatype)
    | VarAssignDecl(datatype,ident,value) -> (ident,datatype)

let get_name_type_val_from_decl decl = match decl with
	VarDecl(datatype, ident) -> (ident, datatype, None)
	| VarAssignDecl(datatype, ident, value) -> (ident, datatype, Some(value))

(* returns tuple (left hand id, left hand id type, right hand value type) *)
let get_name_type_from_var env = function
    VarDecl(datatype,ident) -> (ident,datatype,None)
    | VarAssignDecl(datatype,ident,value) -> (ident,datatype,Some(value))

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
let empty_table_initialization = {parent=None; variables =[];}
let empty_function_table_initialization = {functions=[(Ident("print_string"), Void, [Formal(Datatype(String), Ident("s"))],[]);(Ident("print_int"),Void,[Formal(Datatype(Int),Ident("s"))],[])]}
let empty_environment = {return_type = Datatype(Void); return_seen = false; location="main"; global_scope = empty_table_initialization; var_scope = empty_table_initialization; fun_scope = empty_function_table_initialization}

let find_global_variable env name = 
	try List.find (fun (s,_,_) -> s=name) env.global_scope.variables
	with Not_found -> raise Not_found

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

let find_local_variable env name = 
	try List.find (fun (s,_,_) -> s=name) env.var_scope.variables
	with Not_found -> raise Not_found

(*Semantic checking on a stmt*)
let rec check_stmt env stmt = match stmt with
	| Block(stmt_list) ->
		let new_env=env in
		let getter(env,acc) s =
			let (st, ne) = check_stmt env s in
			(ne, st::acc) in
		let (ls,st) = List.fold_left(fun e s -> getter e s) (new_env,[]) stmt_list in
		let revst = List.rev st in
		(SBlock(revst),ls)
	| Expr(e) -> 
		let _ = check_expr env e in
		(SSExpr(get_sexpr env e),env)
	| Return(e) ->
		let type1=check_expr env e in
		(if not((type1=env.return_type)) then
			raise (Error("Incompatible Return Type")));
		let new_env = {env with return_seen=true} in
		(SReturn(get_sexpr env e),new_env)
	| If(e,s1,s2)->
		let t=get_type_from_datatype(check_expr env e) in
		(if not (t=Boolean) then
			raise (Error("If predicate must be a boolean")));
		let (st1,new_env1)=check_stmt env s1
		and (st2, new_env2)=check_stmt env s2 in
		let ret_seen=(new_env1.return_seen&&new_env2.return_seen) in
		let new_env = {env with return_seen=ret_seen} in
		(SIf((get_sexpr env e),st1,st2),new_env)
	| For(e1,e2,e3,s) ->
		let t1=get_type_from_datatype(check_expr env e1)
		and t2= get_type_from_datatype(check_expr env e2)
		and t3=get_type_from_datatype(check_expr env e3) in
		(if not (t1=Int && t3=Int && t2=Boolean) then
			raise (Error("Improper For loop format")));
		let(st,new_env)=check_stmt env s in
		(SFor((get_sexpr env e1),(get_sexpr env e2), (get_sexpr env e3), st),new_env)
	| While(e,s) ->
		let t=get_type_from_datatype(check_expr env e) in
		(if not(t=Boolean) then
			raise (Error("Improper While loop format")));
		let (st, new_env)=check_stmt env s in
		(SWhile((get_sexpr env e), st),new_env)
	| Ast.Declaration(decl) -> 
		(* If variable is found, multiple decls error
			If variable is not found and var is assigndecl, check for type compat *)
		let (name, ty) = get_name_type_from_decl decl in
		let ((_,dt,_),found) = try (fun f -> ((f env name),true)) find_local_variable with 
			Not_found ->
				((name,ty,None),false) in
		let ret = if(found=false) then
			match decl with
				VarDecl(_,_) ->
					let (sdecl,_) = get_sdecl env decl in
					let (n, t, v) = get_name_type_val_from_decl decl in
					let new_env = add_to_var_table env n t v in
					(SDeclaration(sdecl), new_env)
				| VarAssignDecl(dt, id, value) ->
					let t1 = get_type_from_datatype(dt) and t2 = get_type_from_datatype(get_datatype_from_val env value) in
					if(t1=t2) then
						let (sdecl,_) = get_sdecl env decl in
						let (n, t, v) = get_name_type_val_from_decl decl in
						let new_env = add_to_var_table env n t v in
						(SDeclaration(sdecl), new_env)
					else raise (Error("Type mismatch"))
				else
					raise (Error("Multiple declarations")) in ret
	| Ast.Assign(ident, expr) ->
		(* make sure 1) variable exists, 2) variable and expr have same types *)
		let (_, dt, _) = try find_variable env ident with Not_found -> raise (Error("Uninitialized variable")) in
		let t1 = get_type_from_datatype dt 
		and t2 = get_type_from_datatype(check_expr env expr) in
		if( not(t1=t2) ) then 
			raise (Error("Mismatched type assignments"));
		let sexpr = get_sexpr env expr in
		let new_env = update_variable env (ident,dt,Some((ExprVal(expr)))) in
		(SAssign(SIdent(ident, get_var_scope env ident), sexpr), new_env)
	| Ast.ArrAssign(ident, expr_list) ->
		(* make sure 1) array exists and 2) all types in expr list are equal *)
		let (n,dt,v) = try find_variable env ident with Not_found -> raise (Error("Undeclared array")) in
		let sexpr_list = List.map (fun expr2 -> 
							let expr1 = List.hd expr_list in
							let t1 = get_type_from_datatype(check_expr env expr1) and t2 = get_type_from_datatype(check_expr env expr2) in
							if(t1=t2) then 
								let sexpr2 = get_sexpr env expr2 in sexpr2
								else raise (Error("Array has inconsistent types"))) expr_list in
		let _ = 
			let t1=get_type_from_datatype(check_expr env (List.hd expr_list)) and t2=get_type_from_datatype(dt) in
			if(t1!=t2) then raise (Error("Type Mismatch")) in
		let new_env = update_variable env (n,dt,(Some(ArrVal(expr_list)))) in
		(SArrAssign(SIdent(ident,get_var_scope env ident), sexpr_list), new_env)
	| Ast.ArrElemAssign(ident, expr1, expr2) ->
		(* Make sure
			1) array exists (if it exists, then it was already declared and semantically checked)
			2) expr matches type of array 
			3) index is not out of bounds *)
		let (id, dt, v) = try find_variable env ident with Not_found -> raise (Error("Undeclared array")) in
		let t1 = get_type_from_datatype(dt) and t2 = get_type_from_datatype(check_expr env expr2) in
		let _ = if(t1=t2) then true else raise (Error("Type Mismatch")) in
		let _ = (match v with
				Some(ArrVal(el)) -> (* get_sexpr_list env  *)el
				| None -> raise (Error("No expression on right hand side"))
				| _ -> raise (Error("???"))) in
        let t = get_type_from_datatype(check_expr env expr1) in
        let _ = if not(t=Int) then raise(Error("Array index must be an integer")) in
		(SArrElemAssign(SIdent(ident,get_var_scope env ident), get_sexpr env expr1, get_sexpr env expr2), env)
	| Terminate -> (STerminate, env)

let get_sstmt_list env stmt_list = 
	 List.fold_left (fun (sstmt_list,env) stmt -> 
	 	let (sstmt, new_env) = check_stmt env stmt in 
		(sstmt::sstmt_list, new_env)) ([],env) stmt_list

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
let check_func env func_declaration =
	let new_locals = List.fold_left(fun a vs -> (get_name_type_from_formal env vs)::a)[] func_declaration.formals in
	let new_var_scope = {parent=Some(env.var_scope); variables = new_locals;} in
	let new_env = {return_type = func_declaration.return; return_seen=false; location="in_func"; global_scope = env.global_scope; var_scope = new_var_scope; fun_scope = env.fun_scope} in
	(* let final_env  =List.fold_left(fun env stmt -> snd (check_stmt env stmt)) new_env func_declaration.body in *)
	let (typed_statements, final_env) = get_sstmt_list new_env func_declaration.body in
	let _=check_final_env final_env in
	let sfuncdecl = ({sreturn = func_declaration.return; sfname =
        func_declaration.fname; sformals = func_declaration.formals; sbody =
            typed_statements}) in
	(SFunc_Decl(sfuncdecl,func_declaration.return), env) 

let initialize_functions env function_list = 
	let (typed_functions,last_env) = List.fold_left
		(fun (sfuncdecl_list,env) func-> 	let (sfuncdecl, _) = check_func env func in	
											let final_env = add_function env sfuncdecl in										
											(sfuncdecl::sfuncdecl_list, final_env)) ([],env) function_list in
		(typed_functions,last_env)

(*Semantic checking on a program*)
let check_program program =
	let (functions,(globals,threads)) = program in
	let env = empty_environment in
	let (typed_functions, new_env) = initialize_functions env functions in
	let (typed_globals, new_env2) = List.fold_left(fun (new_globals,env)
             globals -> initialize_globals (new_globals, env) globals) ([], new_env) globals in

	Prog(typed_functions, (typed_globals, typed_threads))
             
