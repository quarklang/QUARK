module A = Ast

type lvalue =
  | Variable of A.datatype * A.ident
  | ArrayElem of A.datatype * A.ident * expr list
(* TODO? add MatrixElem of A.ident * expr * A.datatype *)

and expr =
  | Binop of A.datatype * expr * A.binop * A.datatype * expr
  | AssignOp of A.datatype * lvalue * A.binop * A.datatype * expr
  | Unop of A.unop * A.datatype * expr
  | PostOp of A.datatype * lvalue * A.postop
  | Assign of A.datatype * lvalue * A.datatype * expr
  | IntLit of string
  | BoolLit of string
  | FractionLit of expr * expr
  | QRegLit of expr * expr
  | FloatLit of string
  | ComplexLit of expr * expr
  | StringLit of string
  | ArrayLit of A.datatype * expr list
  | MatrixLit of A.datatype * expr list list
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
