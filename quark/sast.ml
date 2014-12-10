module A = Ast
module T = Type

(* tag what operator is actually used in C++ *)
type op_tag = 
  | OpVerbatim  (* no change to the operator *)
  | CastComplex1 (* cast the first arg to complex *)
  | CastComplex2 (* cast the second arg to complex *)
  | CastFraction1 (* cast the first arg to fraction *)
  | CastFraction2 (* cast the second arg to fraction *)
  | OpArrayConcat
  | OpStringConcat
  | OpMatrixMath (* same as OpVerbatim. conceptual purpose only. *)
  | OpQuerySingleBit (* measure only a single bit, not a range *)

type lvalue =
  | Variable of string
  | ArrayElem of string * expr list
  | MatrixElem of string * expr list

and expr =
  | Binop of expr * A.binop * expr * op_tag
  | Queryop of expr * A.queryop * expr * expr * op_tag (* QuerySingleBit *)
  | Unop of A.unop * expr * op_tag
  | PostOp of lvalue * A.postop
  | Assign of lvalue * expr
  | IntLit of string
  | BoolLit of string
  | FloatLit of string
  | StringLit of string
  | FractionLit of expr * expr
  | QRegLit of expr * expr
  | ComplexLit of expr * expr
  | ArrayLit of A.datatype * expr list
  | ArrayEmpty of A.datatype * expr (* int expr of empty size *)
  | MatrixLit of T.vartype * expr list * int (* column dimension. Flattened *)
  | FunctionCall of string * expr list
  | Lval of lvalue
  | Membership of expr * expr (* not yet supported *)

type decl =
  | AssigningDecl of A.datatype * string * expr
  | PrimitiveDecl of A.datatype * string

type range = Range of expr * expr * expr

type iterator =
    (* first datatype in RangeIterator might be NoneType *)
  | RangeIterator of A.datatype * string * range 
  | ArrayIterator of A.datatype * string * expr

type statement =
  | CompoundStatement of statement list
  | Declaration of decl
  | Expression of expr
  | EmptyStatement
  | IfStatement of expr * statement * statement
  | WhileStatement of expr * statement
  | ForStatement of iterator * statement
  | FunctionDecl of A.datatype * string * decl list * statement list
  | ForwardDecl of A.datatype * string * decl list
  | ReturnStatement of expr
  | VoidReturnStatement
  | BreakStatement
  | ContinueStatement
