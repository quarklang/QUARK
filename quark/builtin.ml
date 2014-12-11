(***** list of built-in functions and their interfaces *****)
module A = Ast
module T = Type

(* system-reserved temporary variable prefix *)
let forbidden_prefix = "_QUARK_TEMP_"

let wrap basic_type = A.DataType(basic_type)

let wrap_list = List.map (fun t -> wrap t) 

(* return arg_types[], return_type *)
let find_builtin = function
  | "print" -> [], wrap T.Void
  | "print_noline" -> [], wrap T.Void
   (* A.NoneType is a placeholder: len works with any array type *)
  | "len" -> [A.ArrayType(A.NoneType)], wrap T.Int
   (* column dimension of a matrix *)
  | "coldim" -> [A.MatrixType(A.NoneType)], wrap T.Int
   (* row dimension of a matrix *)
  | "rowdim" -> [A.MatrixType(A.NoneType)], wrap T.Int
   (* fraction numerator/denominator *)
  | "num" -> [wrap T.Fraction], wrap T.Int
  | "denom" -> [wrap T.Fraction], wrap T.Int
    (* 2nd arg must be a defined function *)
  | "apply_oracle" -> [wrap T.Qreg; wrap T.String; wrap T.Int], wrap T.Void
  
  (* math section *)
  | "sqrt" -> [wrap T.Float], wrap T.Float
  
  (* qureg section *)
  | "qsize" -> [wrap T.Qreg], wrap T.Int
  | "prefix_prob" -> wrap_list [T.Qreg; T.Int; T.Int], wrap T.Float

  (* quantum gate section *)
  | "hadamard" -> [wrap T.Qreg], wrap T.Void
  | "hadamard_top" -> wrap_list [T.Qreg; T.Int], wrap T.Void
  | "pauli_X" -> wrap_list [T.Qreg; T.Int], wrap T.Void
  | "pauli_Y" -> wrap_list [T.Qreg; T.Int], wrap T.Void
  | "pauli_Z" -> wrap_list [T.Qreg; T.Int], wrap T.Void
  | "rot_X" -> wrap_list [T.Qreg; T.Float; T.Int], wrap T.Void
  | "rot_Y" -> wrap_list [T.Qreg; T.Float; T.Int], wrap T.Void
  | "rot_Z" -> wrap_list [T.Qreg; T.Float; T.Int], wrap T.Void
  | "phase_scale" -> wrap_list [T.Qreg; T.Float; T.Int], wrap T.Void
  | "phase_shift" -> wrap_list [T.Qreg; T.Float; T.Int], wrap T.Void
  | "grover_diffuse" -> [wrap T.Qreg], wrap T.Void
  
  | _ -> [], A.NoneType

(* print is special: it accepts any number of args *)
let is_print = function
  | "print" | "print_noline" -> true
  | _ -> false

(* User not allowed to override certain built-in funtions*)
(* because otherwise will cause trouble in codegen  *)
let overridable func = 
  match func with
  | "print" | "print_noline"
  | "apply_oracle" -> 
    failwith @@ "Built-in function " ^func^ "() not overridable"
  | _ -> ()
                       