module A = Ast

type lvalue =
  | Variable of A.ident * A.datatype
  | ArrayElem of A.ident * expr list * A.datatype
(* TODO? add MatrixElem of A.ident * expr * A.datatype *)

and expr =
  | Binop of expr * A.binop * expr * A.datatype
  | AssignOp of lvalue * A.binop * expr * A.datatype
  | Unop of A.unop * expr * A.datatype
  | PostOp of lvalue * A.postop * A.datatype
  | Assign of lvalue * expr * A.datatype
  | IntLit of string
  | BoolLit of string
  | FractionLit of expr * expr
  | QRegLit of expr * expr
  | FloatLit of string
  | ComplexLit of expr * expr
  | StringLit of string
  | ArrayLit of expr list * A.datatype
  | MatrixLit of expr list list * A.datatype
  | Membership of expr * expr * A.datatype
  | FunctionCall of A.ident * expr list
  | Lval of lvalue
  

type decl =
  | AssigningDecl of A.datatype * A.ident * expr
  | PrimitiveDecl of A.datatype * A.ident
  | ArrayDecl of A.datatype * A.ident * expr list

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
