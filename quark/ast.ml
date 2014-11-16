open Type

type binop =
  | Add 
  | Sub 
  | Mul
  | Div 
  | Mod
	| Pow
  | Lshift 
  | Rshift
  | Less 
  | LessEq 
  | Greater
  | GreaterEq 
  | Eq 
  | NotEq
  | BitAnd 
  | BitXor 
  | BitOr 
  | And
  | Or
  | AddEq
  | SubEq
  | MulEq
  | DivEq

type unop = 
  | Neg 
  | Not 
  | BitNot

type postop = 
  | Dec 
  | Inc

type datatype =
  | DataType of Type.var_type
  | ArrayType of datatype

type ident = Ident of string

type lvalue =
  | Variable of ident
  | ArrayElem of ident * expr list

and expr =
  | Binop of expr * binop * expr
  | AssignOp of lvalue * binop * expr
  | Unop of unop * expr
  | PostOp of lvalue * postop
  | Assign of lvalue * expr
  | IntLit of string
  | BoolLit of string
  | FractionLit of expr * expr
  | QRegLit of expr * expr
  | FloatLit of string
  | StringLit of string
  | ArrayLit of expr list
  | ComplexLit of expr * expr
  | Cast of datatype * expr
  | FunctionCall of ident * expr list
  | Lval of lvalue

type decl =
  | AssigningDecl of datatype * ident * expr
  | PrimitiveDecl of datatype * ident

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

(* let type_of_string = function
  | "int" -> Int
  | "float" -> Float
  | "bool" -> Bool
  | "complex" -> Complex
  | "fraction" -> Fraction
  | "qreg" -> QReg
  | "string" -> String
  | "void" -> Void
  | dtype -> raise (Invalid_type dtype) *)

(* type top_level = 
   *)