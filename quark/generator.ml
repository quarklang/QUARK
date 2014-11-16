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
  let paramStr = 
    List.fold_left (fun s param -> s ^ (gen_param param)) "" paramList
  in
  (* get rid of the last 2 chars ', ' *)
  if paramStr = "" then ""
  else
    String.sub paramStr 0 ((String.length paramStr) - 2)
	
let rec gen_range id = function
	| Range(exStart, exEnd, exStep) -> 
    let exStart = gen_expr exStart in
    let exEnd = gen_expr exEnd in
    let exStep = gen_expr exStep in
    let exCmp = "(" ^exEnd^ ")>(" ^exStart^ ") ? " in
    let id = gen_id id in
      "(" ^id^ "=" ^exStart^ "; " ^
      exCmp^ id^" < (" ^exEnd^ ") : " ^id^ " > (" ^exEnd^ "); " ^
      exCmp^ id^" += (" ^exStep^ ") : " ^id^ " -= (" ^exStep^ "))"
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
      | FunctionDecl(returnTyp, funcId, paramList, stmtList) ->
        let funcId = gen_id funcId in
          begin
            print_endline @@ (gen_datatype returnTyp) ^ " " ^ 
              funcId ^ "(" ^ (gen_param_list paramList) ^ ")";
            print_endline @@ "{ // start " ^ funcId;
            eval stmtList;
            print_endline @@ "} // end " ^ funcId ^ "\n";
          end
      
      (* TODO: get rid of forward decl *)
      | ForwardDecl(returnTyp, funcId, paramList) -> 
          print_endline @@ "*forward* " ^ (gen_datatype returnTyp) ^ " " ^ 
            (gen_id funcId) ^ "(" ^ (gen_param_list paramList) ^ ");\n";

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
      | _ -> failwith "nothing for eval()"
    end;
    eval rest

let _ =
	let lexbuf = Lexing.from_channel stdin in
	eval @@ Parser.top_level Scanner.token lexbuf
