open Ast
open Type

type postop = 
  | Dec 
  | Inc

type ident = Ident of Ast.ident

type lvalue =
  | Variable of ident * datatype
  | ArrayElem of ident * expr list
  (* | ComplexAccess of expr * ident *)
and expr =
  | Binop of expr * binop * expr * datatype
  | AssignOp of lvalue * binop * expr * datatype
  | Unop of unop * expr * datatype
  | PostOp of lvalue * postop * datatype
  | Assign of lvalue * expr * datatype
  | IntLit of int * datatype
  | BoolLit of bool * datatype
  | FractionLit of expr * expr * datatype
  | QRegLit of int * int * datatype
  | FloatLit of float * datatype
  | ComplexLit of float * float * datatype
  | StringLit of string * datatype
  | ArrayLit of expr list * datatype
  | Cast of datatype * expr
  | FunctionCall of ident * expr list
  | Lval of lvalue

type decl =
  | AssigningDecl of ident * expr
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
  | PforStatement of iterator list * statement
  | FunctionDecl of bool * datatype * ident * decl list * statement list
  | ForwardDecl of bool * datatype * ident * decl list
  | ReturnStatement of expr
  | VoidReturnStatement
