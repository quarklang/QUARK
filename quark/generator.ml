open Ast
open Printf
open Type

let print = "print"

let imports =
  "#include \"qureg.h\"\n" ^
   "#include \"qumat.h\"\n" ^
   "#include \"qugate.h\"\n" ^
   "#include \"algor.h\"\n"

let top = "\nint main(void) { \n"
let bottom = "\n}\n"

let gen_id (Ident name) = name

let gen_unop = function
  Neg -> "-"
| Not -> "!"
| _ -> "misc"

let gen_postop = function
  Inc -> "++"
| Dec -> "--"

let gen_binop = function
  Add -> "+"
| Sub -> "-"
| Mul -> "*"
| Div -> "/"
| Mod -> "%"
| Pow -> "**"
| Lshift -> "<<"
| Rshift -> ">>"
| Less -> "<"
| LessEq -> "<="
| Greater -> ">"
| GreaterEq -> ">="
| Eq -> "=="
| NotEq -> "!="
| BitAnd -> "&"
| BitXor -> "^"
| BitOr -> "|"
| And -> "&&"
| Or -> "||"

let rec gen_datatype = function
	| DataType(t) -> 
		(match t with
    | Int -> "int"
    | Float -> "float"
    | Bool -> "bool"
    | Fraction -> "Frac"
    | Complex -> "CX"
    | QReg -> "Qureg"
    | String -> "string"
    | Void -> "void"
    | _ -> failwith "datatype fatal error")
	| ArrayType(t) -> 
		(gen_datatype t) ^ "[]"


let rec gen_expr = function
	| _ -> "expr"

let rec gen_param = function 
  | PrimitiveDecl(typ, id) -> 
		(gen_datatype typ) ^ " " ^ (gen_id id) ^ ", "
  | _ -> failwith "decl list fatal error"

let rec gen_param_list paramList =
  List.fold_left (fun s param -> s ^ (gen_param param)) "" paramList
	
let rec gen_range id = function
	| Range(IntLit(iStart), IntLit(iEnd), IntLit(iStep)) -> 
    let dir = if iEnd >= iStart then "<" else ">" in
    let update = if iEnd >= iStart then "+=" else "-=" in
    let iStart = string_of_int iStart in
    let iEnd = string_of_int iEnd in
    if iStep <= 0 then failwith "range step must a positive int"
    else
      let iStep = string_of_int iStep in
      let id = gen_id id in
      "(" ^id^"="^iStart^"; " ^id^dir^iEnd^"; " ^id^update^iStep ^")"
  | _ -> failwith "range fatal error"
	
let rec gen_iterator_list = function
	| [] -> ""
	| item :: rest -> 
    (match item with
    | RangeIterator(id, rng) -> gen_range id rng
    | ArrayIterator(id, ex) -> 
      (gen_id id) ^ " in " ^ (gen_expr ex)) ^ 
      (gen_iterator_list rest)
	
let rec gen_decl = function
  | AssigningDecl(typ, id, expr) -> 
    (gen_datatype typ) ^ " " ^ (gen_id id) ^ " = " ^ (gen_expr expr)
  | PrimitiveDecl(typ, id) -> 
    (gen_datatype typ) ^ " " ^ (gen_id id)

let rec eval stmts =
  match stmts with
  | [] -> ()
  | stmt :: rest ->
    begin
      match stmt with
			(* top level statements *)
      | FunctionDecl(returnType, funcId, paramList, stmtList) ->
        begin
          print_endline @@ (gen_id funcId) ^ ": " ^ (gen_param_list paramList);
          eval stmtList;
        end

			(* statements *)
			| IfStatement(ex, stmtIf, stmtElse) -> 
				begin
					print_endline @@ "if (" ^ gen_expr(ex) ^ ")";
					print_endline "{ // start if";
					eval [stmtIf];
					print_endline "else";
					eval [stmtElse];
					print_endline "} // end if";
				end
				
			| WhileStatement(ex, stmt) -> 
				begin
					print_endline @@ "while (" ^ gen_expr(ex) ^ ")";
					print_endline "{ // start while";
					eval [stmt];
					print_endline "} // end while";
				end
				
			| ForStatement(iterList, stmt) -> 
				begin
					print_endline @@ "for " ^ gen_iterator_list(iterList);
					print_endline "{ // start for";
					eval [stmt];
					print_endline "} // end for";
				end
				
			| CompoundStatement(stmtList) -> 
				begin
					print_endline "{ // start compound";
					eval stmtList;
					print_endline "} // end compound";
				end

      | Declaration(dec) -> print_endline @@ (gen_decl dec) ^ ";"
			| Expression(ex) -> print_endline @@ (gen_expr ex) ^ ";"
			| EmptyStatement -> print_endline ";"
      | VoidReturnStatement -> print_endline "return; // void"
      | ReturnStatement(ex) -> 
				print_endline @@ "return " ^ (gen_expr ex) ^ ";"
      | _ -> print_endline "..."
    end;
    eval rest

let _ =
	let lexbuf = Lexing.from_channel stdin in
	eval @@ Parser.top_level Scanner.token lexbuf
