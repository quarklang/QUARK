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

let header = 
  "#include <stdio.h>\n\
  #include <stdlib.h>\n\
  #include <stdint.h>\n\n"

let gen_id = function
  | Ident(name) -> name
  | _ -> failwith "ident print error"

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
    | Fraction -> "frac"
    | Complex -> "complex"
    | QReg -> "qreg"
    | String -> "string"
    | Void -> "void"
    | _ -> "misc")
	| ArrayType(t) -> 
		(gen_datatype t) ^ "[]"


let rec gen_expr = function
	| _ -> "expr"

let rec gen_param = function 
  | PrimitiveDecl(datatyp, id) -> 
		(gen_datatype datatyp) ^ " " ^ (gen_id id) ^ ", "
  | _ -> failwith "decl list error"

let rec gen_param_list paramList =
  List.fold_left (fun s param -> s ^ (gen_param param)) "" paramList

let rec eval stmts =
  match stmts with
  | [] -> print_endline "end"
  | stmt :: rest ->
    begin
      match stmt with
      | FunctionDecl(b, returnType, funcName, paramList, stmtList) ->
        begin
          print_endline @@ gen_param_list paramList;
          eval stmtList;
        end
      | VoidReturnStatement -> print_endline "void_return"
      | ReturnStatement(returnExpr) -> 
				print_endline @@ "return " ^ (gen_expr returnExpr)
      | _ -> print_endline "...";
      eval rest;
    end

let _ =
	let lexbuf = Lexing.from_channel stdin in
	let expr = Parser.top_level Scanner.token lexbuf in
    eval expr
