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
  | Variable of A.datatype * A.ident
  | ArrayElem of A.datatype * A.ident * expr list
(* TODO? add MatrixElem of A.ident * expr * A.datatype *)

and expr =
  | Binop of expr * A.binop * expr * op_tag
  | Queryop of expr * A.queryop * expr * expr * op_tag (* QuerySingleBit *)
  | AssignOp of A.datatype * lvalue * A.binop * A.datatype * expr
  | Unop of A.unop * expr * op_tag
  | PostOp of A.datatype * lvalue * A.postop
  | Assign of A.datatype * lvalue * A.datatype * expr
  | IntLit of string
  | BoolLit of string
  | FloatLit of string
  | StringLit of string
  | FractionLit of expr * expr
  | QRegLit of expr * expr
  | ComplexLit of expr * expr
  | ArrayLit of A.datatype * expr list
  | MatrixLit of T.vartype * expr list list * int (* column dimension *)
  | Membership of A.datatype * expr * A.datatype * expr
  | FunctionCall of A.ident * expr list
  | Lval of lvalue

type decl =
  | AssigningDecl of A.datatype * A.ident * expr
  | PrimitiveDecl of A.datatype * A.ident
  (* | ArrayDecl of A.datatype * A.ident * expr list *)

type range = Range of expr * expr * expr

type iterator =
  | RangeIterator of A.ident * range
  | ArrayIterator of A.ident * expr

type statement =
  | CompoundStatement of statement list
  | Declaration of decl
  | Expression of expr
  | EmptyStatement
  | IfStatement of expr * statement * statement
  | WhileStatement of expr * statement
  | ForStatement of iterator * statement
  | FunctionDecl of A.datatype * A.ident * decl list * statement list
  | ForwardDecl of A.datatype * A.ident * decl list
  | ReturnStatement of expr
  | VoidReturnStatement
  | BreakStatement
  | ContinueStatement
