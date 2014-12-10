(***** list of built-in functions and their interfaces *****)
module A = Ast
module T = Type

let wrap basic_type = A.DataType(basic_type)

(* return arg_types[], return_type *)
let find_builtin = function
   (* print is special: it accepts any number of args *)
  | "print" -> [], wrap T.Void
   (* A.NoneType is a placeholder: len works with any array type *)
  | "len" -> [A.ArrayType(A.NoneType)], wrap T.Int
   (* size of a qureg *)
  | "qsize" -> [A.DataType(T.QReg)], wrap T.Int
   (* column dimension of a matrix *)
  | "coldim" -> [A.MatrixType(A.NoneType)], wrap T.Int
   (* row dimension of a matrix *)
  | "rowdim" -> [A.MatrixType(A.NoneType)], wrap T.Int
  | _ -> [], A.NoneType