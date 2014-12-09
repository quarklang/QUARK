type vartype =
  | Int
  | Float
  | Bool
  | Fraction
  | Complex
  | QReg
  | String
  | Void

let str_of_type = function
  | Int -> "int"
  | Float -> "float"
  | Bool -> "bool"
  | Fraction -> "fraction"
  | Complex -> "complex"
  | QReg -> "qreg"
  | String -> "string"
  | Void -> "void"