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

let rec eval tree = 
  match tree with
    | [IntLit(x)] -> x
    | _ -> 2

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let expr = Parser.top_level Scanner.token lexbuf in
  expr

  (* let result = eval expr in
  print_string header;
  print_endline (string_of_int result);
  print_string bottom; *)
