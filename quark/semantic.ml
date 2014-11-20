open Ast
open Sast
open Type

type translation_environment = {
  scope : symbol_table; (* symbol table for vars *)
  return_type : Types.t;
  in_switch : bool;
  case_labels : Big_int.big_int list ref; (* known case labels *)
  break_label : label option; (* when break makes sense *)
  continue_label : label option; (* when continue makes sense *)
  exception_scope : exception_scope; (* sym tab for exceptions *)
  labels : label list ref; (* labels on statements *)
  forward_gotos : label list ref; (* forward goto destinations *)
}

type symbol_table = {
  parent : symbol_table option; variables : variable_decl list
}

let rec find_variable (scope : symbol_table) name =
    try
      List.find (fun (s, _, _, _) -> s = name) scope.variables
    with Not_found ->
      match scope.parent with
          Some(parent) -> find_variable parent name
        | _ -> raise Not_found

(* Information about where we are *)
type translation_environment = {
  scope : symbol_table;
}

let rec expr env = function
    (* An integer constant: convert and return Int type *)
    Ast.IntConst(v) -> Sast.IntConst(v), Types.Int
    (* An identifier: verify it is in scope and return its type *)
  | Ast.Id(vname) ->
      let vdecl = try
        find_variable env.scope vname (* locate a variable by name *)
      with Not_found ->
        raise (Error("undeclared identifier " ^ vname))
      in
      let (_, typ) = vdecl in (* get the variable type *)
      Sast.Id(vdecl), typ
    (* let rec expr env = function *)
  | A.BinOp(e1, op, e2) ->
      (* Check left and right children *)
      let e1 = expr env e1 
      and e2 = expr env e2 in

      (* Get the type of each child *)
      let _, t1 = e1
      and _, t2 = e2 in

      if op <> Ast.Equal && op <> Ast.NotEqual then
        (* Most operators require both left and right to be integer *)
        (require_integer e1 "Left operand must be integer";
         require_integer e2 "Right operand must be integer")
      else
        if not (weak_eq_type t1 t2) then
          (* Equality operators just require types to be "close" *)
          error ("Type mismatch in comparison: \"" ^ 
                 "left is "    ^ Printer.string_of_sast_type t1 ^ "\" " ^ 
                 "right is \"" ^ Printer.string_of_sast_type t2 ^ "\""
                ) loc;

      (* Success: result is int *)
      Sast.BinOp(e1, op, e2), Types.Int

let rec stmt env = function
    (* Expression statement: just check the expression *)
    Ast.Expression(e) -> Sast.Expression(expr env e)

    (* If statement: verify the predicate is integer *)
  | Ast.If(e, s1, s2) ->
    (* Check the predicate *)
    let e = check_expr env e in
    require_integer e "Predicate of if must be integer";
    (* Check then, else *)
    Sast.If(e, stmt env s1, stmt env s2)

  | A.Local(vdecl) ->
    let decl, (init, _) = check_local vdecl (* already declared? *) in

    (* Side-effect: add variable to the environment *)
    env.scope.S.variables <- decl :: env.scope.S.variables;
    init (* initialization statements, if any *)

  | A.Block(sl) ->
    (* New scopes: parent is the existing scope, start out empty *)
    let scope' = { S.parent = Some(env.scope); S.variables = [] }
    and exceptions' = {
      excep_parent = Some(env.exception_scope); exceptions = []
    } in

    (* New environment: same, but with new symbol tables *)
    let env = {
        env with scope = scope';
        exception_scope = exceptions'
    } in

    (* Check all the statements in the block *)
    let sl = List.map (fun s -> stmt env s) sl in
    scope.S.variables <-
      List.rev scope.S.variables; (* side-effect *)

    (* Success: return block with symbols *)
    Sast.Block(scope, sl)
