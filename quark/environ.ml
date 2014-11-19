open Ast

(*
 * When a new scope is entered, we push to the VarMap stack; when a scope
 * is left, we pop.
 * *)

(* Maps variable idents to type *)
module VarMap = Map.Make(struct type t = ident let compare = compare end);;

(* Maps function idents to return type *)
module FuncMap = Map.Make(struct type t = ident let compare = compare end);;

module FuncDeclMap = Map.Make(struct type t = ident let compare = compare end);;

exception Already_declared
exception Invalid_environment
exception Invalid_operation
exception Not_implemented
exception Symbol_not_found of string

type 'a env = {
  func_type_map: (datatype * datatype list) FuncMap.t;
  scope_stack: datatype VarMap.t list;
}

type 'a sourcecomponent =
  | Verbatim of string
  | Generator of ('a -> (string * 'a))
  | NewScopeGenerator of ('a -> (string * 'a))

let create =
{
  func_type_map = FuncMap.empty;
  scope_stack = VarMap.empty ::[];
}

let update_env func_type_map var_map_list =
{
  func_type_map = func_type_map;
  scope_stack = var_map_list;
}

let var_in_scope ident env =
  let rec check_scope scopes =
    match scopes with
      | [] -> false
      | scope :: tail ->
        if VarMap.mem ident scope then
            true
        else check_scope tail
  in check_scope env.scope_stack

let get_var_type ident env =
  let rec check_scope scopes =
    match scopes with
     | [] -> let Ident(sym) = ident in raise (Symbol_not_found sym)
     | scope :: tail ->
         if VarMap.mem ident scope then
           VarMap.find ident scope
         else
           check_scope tail in
  let scope_stack = env.scope_stack in
  check_scope scope_stack

let is_var_declared ident env =
  match env.scope_stack with
   | [] -> false
   | scope :: tail -> VarMap.mem ident scope

let set_var_type ident datatype env =
  let scope, tail = (match env.scope_stack with
                | scope :: tail -> scope, tail
                | [] -> raise Invalid_environment) in
  let new_scope = VarMap.add ident datatype scope in
  update_env env.func_type_map (new_scope :: tail)

let update_scope ident datatype (str, env) =
  if is_var_declared ident env then
    raise Already_declared
  else
    (str, set_var_type ident datatype env)

let push_scope env =
  update_env env.func_type_map 
    (VarMap.empty :: env.scope_stack)

let pop_scope env =
  match env.scope_stack with
   | local_scope :: tail ->
      update_env env.func_type_map tail
   | [] -> raise Invalid_environment

let get_func_info ident env =
    FuncMap.find ident env.func_type_map

let is_func_declared ident env =
  FuncMap.mem ident env.func_type_map

let set_func_type ident returntype arg_list env =
  let new_func_type_map =
      FuncMap.add ident (returntype, arg_list) env.func_type_map in
  update_env new_func_type_map env.scope_stack

let update_functions ident returntype arg_list (str, env) =
  if is_func_declared ident env then
    raise Already_declared
  else
    (str, set_func_type ident returntype arg_list env)

let combine initial_env components =
    let f (str, env) component =
        match component with
         | Verbatim(verbatim) -> str ^ verbatim, env
         | Generator(gen) ->
           let new_str, new_env = gen env in
            str ^ new_str, new_env
         | NewScopeGenerator(gen) ->
           let new_str, new_env = gen (push_scope env) in
            str ^ new_str, pop_scope new_env in
    List.fold_left f ("", initial_env) components
