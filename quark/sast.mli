module A = Ast

type ident = Ident of A.ident

type lvalue =
  | Variable of ident * datatype
  | ArrayElem of ident * expr list * datatype
(* TODO? add MatrixElem of ident * expr * datatype *)

and expr =
  | Binop of expr * binop * expr * datatype
  | AssignOp of lvalue * binop * expr * datatype
  | Unop of unop * expr * datatype
  | PostOp of lvalue * A.postop * datatype
  | Assign of lvalue * expr * datatype
  | IntLit of string * datatype
  | BoolLit of string * datatype
  | FractionLit of expr * expr * datatype
  | QRegLit of string * string * datatype
  | FloatLit of string * datatype
  | ComplexLit of string * string * datatype
  | StringLit of string * datatype
  | ArrayLit of expr list * datatype
  | Cast of datatype * expr * datatype
  | FunctionCall of ident * expr list
  | Lval of lvalue

type decl =
  | AssigningDecl of datatype * ident * expr
  | PrimitiveDecl of datatype * ident
  | ArrayDecl of datatype * ident * expr list

type range = Range of expr * expr * expr

type iterator =
  | RangeIterator of ident * range
  | ArrayIterator of ident * expr

type statement =
  | CompoundStatement of statement list
  | Declaration of decl
  | Expression of expr
  | EmptyStatement
  | IfStatement of expr * statement * statement
  | WhileStatement of expr * statement
  | ForStatement of iterator list * statement
  | FunctionDecl of datatype * ident * decl list * statement list
  | ForwardDecl of datatype * ident * decl list
  | ReturnStatement of expr
  | VoidReturnStatement
