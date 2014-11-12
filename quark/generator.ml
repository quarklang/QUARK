open Ast

let print = "print"

let imports =
  "#include \"qureg.h\"\n" ^
   "#include \"qumat.h\"\n" ^
   "#include \"qugate.h\"\n" ^
   "#include \"algor.h\"\n"

let gen_id = function
  Ident(id) -> id

let gen_unop = function
  Neg -> "-"
| Not -> "!"
| _ -> "misc"

let gen_postop = function
  Inc -> "++"
| Dec -> "--"
| _ -> "misc"

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
| _ -> "misc"

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

let rec eval = function
    IntLit(x) -> print_int x
  | _ -> print_string "234234"

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let expr = Parser.top_level Scanner.token lexbuf in
  let result = eval expr in
  print_endline (string_of_int result)
