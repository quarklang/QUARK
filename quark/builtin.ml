(***** list of built-in functions and their interfaces *****)
module A = Ast
module T = Type

(* system-reserved temporary variable prefix *)
let forbidden_prefix = "_QUARK_TEMP_"

let wrap basic_type = A.DataType(basic_type)

(* NoneType is a placeholder: len works with any array type *)
let any_array = A.ArrayType(A.NoneType)
(* NoneType is a placeholder, works with any matrix type *)
let any_matrix = A.MatrixType(A.NoneType)

let cx_mat = A.MatrixType(wrap T.Complex)
let i = wrap T.Int
let f = wrap T.Float
let b = wrap T.Bool
let frac = wrap T.Fraction
let cx = wrap T.Complex
let qreg = wrap T.Qreg
let s = wrap T.String
let void = wrap T.Void

(* return arg_types[], return_type *)
let find_builtin = function
  | "print" -> [], void
  | "print_noline" -> [], void
  | "len" -> [any_array], i
   (* fraction numerator/denominator *)
  | "num" -> [frac], i
  | "denom" -> [frac], i
  (***** math section *****)
  | "sqrt" -> [f], f
  | "rand_int" -> [i; i], i
  | "rand_float" -> [f; f], f
  
  (***** matrix section *****)
  | "coldim" -> [any_matrix], i
  | "rowdim" -> [any_matrix], i
    (* matrix generation *)
  | "hadamard_mat" -> [i], cx_mat
  | "cnot_mat" -> [], cx_mat
  | "toffoli_mat" -> [i], cx_mat
  | "generic_control_mat" -> [i; cx_mat], cx_mat
  | "pauli_X_mat" -> [], cx_mat
  | "pauli_Y_mat" -> [], cx_mat
  | "pauli_Z_mat" -> [], cx_mat
  | "rot_X_mat" -> [f], cx_mat
  | "rot_Y_mat" -> [f], cx_mat
  | "rot_Z_mat" -> [f], cx_mat
  | "phase_scale_mat" -> [f], cx_mat
  | "phase_shift_mat" -> [f], cx_mat
  | "control_phase_shift_mat" -> [f], cx_mat
  | "swap_mat" -> [], cx_mat
  | "cswap_mat" -> [], cx_mat
  | "qft_mat" -> [i], cx_mat
  | "grover_diffuse_mat" -> [i], cx_mat

  (***** qureg section *****)
  | "qsize" -> [qreg], i
  | "qclone" -> [qreg], qreg
  | "prefix_prob" -> [qreg; i; i], f
    (* 2nd arg must be a defined function *)
  | "apply_oracle" -> [qreg; s; i], void

  (***** quantum gate section *****)
    (* single-bit gates *)
  | "hadamard" -> [qreg], void
  | "hadamard_top" -> [qreg; i], void
  | "pauli_X" -> [qreg; i], void
  | "pauli_Y" -> [qreg; i], void
  | "pauli_Z" -> [qreg; i], void
  | "rot_X" -> [qreg; f; i], void
  | "rot_Y" -> [qreg; f; i], void
  | "rot_Z" -> [qreg; f; i], void
  | "phase_scale" -> [qreg; f; i], void
  | "phase_shift" -> [qreg; f; i], void
    (* multi-bit gates *)
  | "generic_1gate" -> [qreg; cx_mat; i], void
  | "generic_2gate" -> [qreg; cx_mat; i; i], void
  | "generic_ngate" -> [qreg; cx_mat; A.ArrayType(i)], void
    (* control gates *)
  | "cnot" -> [qreg; i; i], void
  | "toffoli" -> [qreg; i; i; i], void
  | "control_phase_shift" -> [qreg; f; i; i], void
  | "ncnot" -> [qreg; A.ArrayType(i); i], void
  | "generic_control" -> [qreg; cx_mat; i; i], void
  | "generic_toffoli" -> [qreg; cx_mat; i; i; i], void
  | "generic_ncontrol" -> [qreg; cx_mat; A.ArrayType(i); i], void
    (* other gates *)
  | "swap" -> [qreg; i; i], void
  | "cswap" -> [qreg; i; i; i], void
  | "qft" -> [qreg; i; i], void
  | "grover_diffuse" -> [qreg], void
  | _ -> ([], A.NoneType)

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