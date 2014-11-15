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
  Ident(id) -> id

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

let gen_datatype = function
  Int -> "int"
| Float -> "float"
| Bool -> "bool"
| Fraction -> "frac"
| Complex -> "complex"
| QReg -> "qreg"
| String -> "std::string"
| Void -> "void"
| _ -> "misc"

(* let rec eval tree = function
    IntLit(x) -> print_int x
  | _ -> print_string "234234"
 *)


let str_id id =
  match id with 
  | Ident(name) -> name
  | _ -> failwith "ident print error"

let rec decl_list_str declList =
  List.fold_left (fun s declItem ->
    match declItem with
    | PrimitiveDecl(datatyp, id) -> s ^ (str_id id) ^ ", "
    | _ -> failwith "error") ""
  declList

let rec eval stmts =
  match stmts with
  | [] -> print_endline "end"
  | stmt :: rest ->
    begin
      match stmt with
      | FunctionDecl(b, returnType, funcName, declList, stmtList) ->
      begin
        print_endline @@ decl_list_str declList;
        eval stmtList;
      end
      | VoidReturnStatement -> print_endline "void_return"
      | _ -> print_endline "...";
      eval rest;
    end

let _ =
	let lexbuf = Lexing.from_channel stdin in
	let expr = Parser.top_level Scanner.token lexbuf in
    eval expr