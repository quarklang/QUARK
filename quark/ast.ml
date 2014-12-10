module T = Type

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
  | BitAndEq

type queryop =
  | Query
  | QueryUnreal

type unop = 
  | Neg 
  | Not 
  | BitNot

type postop = 
  | Dec 
  | Inc

type datatype = 
  | DataType of T.vartype
  | ArrayType of datatype
  | MatrixType of datatype
  | NoneType (* if a symbol doesn't exist *)

type ident = Ident of string

type lvalue =
  | Variable of ident
  | ArrayElem of ident * expr list

and expr =
  | Binop of expr * binop * expr
  | AssignOp of lvalue * binop * expr
  | Queryop of expr * queryop * expr * expr
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
  | ArrayCtor of datatype * expr
  | MatrixLit of expr list list
  | MatrixCtor of datatype * expr * expr
  | ComplexLit of expr * expr
  | Lval of lvalue
  | Membership of expr * expr
  | FunctionCall of ident * expr list

type decl =
  | PrimitiveDecl of datatype * ident
  | AssigningDecl of datatype * ident * expr

type range = Range of expr * expr * expr

type iterator =
  | RangeIterator of datatype * ident * range
  | ArrayIterator of datatype * ident * expr

type statement =
  | CompoundStatement of statement list
  | Declaration of decl
  | Expression of expr
  | EmptyStatement
  | IfStatement of expr * statement * statement
  | WhileStatement of expr * statement
  | ForStatement of iterator * statement
  | FunctionDecl of datatype * ident * decl list * statement list
  | ForwardDecl of datatype * ident * decl list
  | ReturnStatement of expr
  | VoidReturnStatement
  | BreakStatement
  | ContinueStatement


let rec str_of_datatype = function
	| DataType(t) -> 
    T.str_of_type t
	| ArrayType(t) -> 
		str_of_datatype t ^ "[]"
	| MatrixType(t) -> (
    match t with
    | DataType(elem_type) -> ( 
      match elem_type with
      (* only support 3 numerical types *)
      | T.Int | T.Float | T.Complex -> 
      "[|" ^ T.str_of_type elem_type ^ "|]"
      | _ -> failwith "INTERNAL non-numerical matrix type to str"
      )
    | NoneType -> "[|AnyType|]"
    (* we shouldn't support float[][[]] *)
    | _ -> 
      failwith "INTERNAL bad matrix type to str"
    )
  | NoneType -> "AnyType"

let str_of_binop = function
| Add -> "+"
| Sub -> "-"
| Mul -> "*"
| Div -> "/"
| Mod -> "mod"
| Pow -> "**"
| Lshift -> "<<"
| Rshift -> ">>"
| Less -> "<"
| LessEq -> "<="
| Greater -> ">"
| GreaterEq -> ">="
| Eq -> "=="
| NotEq -> "!="
| BitAnd -> "&"
| BitXor -> "^"
| BitOr -> "|"
| And -> "and"
| Or -> "or"
| AddEq -> "+="
| SubEq -> "-="
| MulEq -> "*="
| DivEq -> "/="
| BitAndEq -> "&="
| _ -> failwith "INTERNAL unhandled binop"

let str_of_unop = function
| Neg -> "-"
| Not -> "not"
| BitNot -> "~"
| _ -> failwith "INTERNAL unhandled unop"

let str_of_postop = function
| Inc -> "++"
| Dec -> "--"
| _ -> failwith "INTERNAL unhandled postop"