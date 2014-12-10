type vartype =
  | Int
  | Float
  | Bool
  | Fraction
  | Complex
  | Qreg
  | String
  | Void

let str_of_type = function
  | Int -> "int"
  | Float -> "float"
  | Bool -> "bool"
  | Fraction -> "fraction"
  | Complex -> "complex"
  | Qreg -> "qreg"
  | String -> "string"
  | Void -> "void"