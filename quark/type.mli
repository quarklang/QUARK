type t =
  | Int
  | Float
  | Bool
  | Fraction
  | Complex
  | QReg
  | String
  | Void

(* helper function *)
let type_of_string = function
  | "int"       -> Int
  | "float"     -> Float
  | "bool"      -> Bool
  | "fraction"  -> Fraction
  | "complex"   -> Complex
  | "qreg"      -> QReg
  | "string"    -> String
  | "void"      -> Void
  | dtype       -> raise (Invalid_type dtype)
