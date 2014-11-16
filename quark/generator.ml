open Ast
open Printf
open Type

let print = "print"

let header =
  "#include \"qureg.h\"\n" ^
   "#include \"qumat.h\"\n" ^
   "#include \"qugate.h\"\n" ^
   "using namespace Qumat;\n" ^
   "using namespace Qugate;\n"

let top = "\nint main(void) { \n"
let bottom = "\n}\n"

(* surround with parenthesis *)
let surr str = "(" ^ str ^ ")"

let gen_id (Ident name) = name

let gen_unop = function
  Neg -> "-"
| Not -> "!"
| BitNot -> "~"

let gen_postop = function
  Inc -> "++"
| Dec -> "--"

let gen_binop = function
  Add -> "+"
| Sub -> "-"
| Mul -> "*"
| Div -> "/"
| Mod -> "%"
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
| And -> "&&"
| Or -> "||"
| AddEq -> "+="
| SubEq -> "-="
| MulEq -> "*="
| DivEq -> "/="
| _ -> failwith "unhandled binop"


let rec gen_datatype = function
	| DataType(t) -> 
		(match t with
    | Int -> "int"
    | Float -> "float"
    | Bool -> "bool"
    | Fraction -> "Frac"
    | Complex -> "complex<float>"
    | QReg -> "Qureg"
    | String -> "string"
    | Void -> "void")
	| ArrayType(t) -> 
		(gen_datatype t) ^ "[]"


let rec gen_expr = function
  (* literals *)
  | IntLit(x) | FloatLit(x) | BoolLit(x) -> 
    x
  | StringLit(s) -> 
    "\"" ^ s ^ "\""
  | ArrayLit(exlist) -> 
    "[" ^ gen_expr_list exlist ^ "]"
  | FractionLit(exNum, exDenom) -> 
    "Frac(" ^ gen_expr exNum ^ ", " ^ gen_expr exDenom ^ ")"
  | QRegLit(ex1, ex2) -> 
    "Qureg::create<true>(" ^ gen_expr ex1 ^ ", " ^ gen_expr ex2 ^ ")"
  | ComplexLit(exReal, exImag) -> 
    "complex<float>(" ^ gen_expr exReal ^ ", " ^ gen_expr exImag ^ ")"
  
  (* Binary ops *)
  | Binop(ex1, op, ex2) -> 
    let ex1 = gen_expr ex1 in
    let ex2 = gen_expr ex2 in
     (match op with
      | Query -> "measure_top(" ^ex1^ ", " ^ex2^ ", true)"
      | QueryUnreal -> "measure_top(" ^ex1^ ", " ^ex2^ ", false)"
      | _ -> ex1 ^" "^ gen_binop op ^" "^ ex2)
  
  (* Unary ops *)
  | Unop(op, ex) -> 
    gen_unop op ^ gen_expr ex
  
  (* Assignment *)
  | Assign(lval, ex) -> 
    gen_lvalue lval ^ " = " ^ gen_expr ex
  | Lval(lval) -> 
    gen_lvalue lval
  
  (* Special assignment *)
  | AssignOp(lval, op, ex) -> 
    gen_lvalue lval ^" "^ gen_binop op ^" "^ gen_expr ex
  | PostOp(lval, op) -> 
    gen_lvalue lval ^" "^ gen_postop op
    
  (* Membership testing with keyword 'in' *)
  | Membership(exElem, exArray) -> 
    (* !!!! Needs to assign exElem and exArray to compiled temp vars *)
    (* Shouldn't change over calls!!! *)
    let exElem = gen_expr exElem in
    let exArray = gen_expr exArray in
      "std::find(" ^surr exArray^ ".begin(), " ^surr exArray^ ".end(), " ^
      exElem^ ") != " ^surr exArray^ ".end()"
    
  (* Function calls *)
  | FunctionCall(funcId, exlist) -> 
    gen_id funcId ^ surr( gen_expr_list exlist )
  
  | _ -> failwith "some expr not parsed"

and gen_expr_list exlist =
  let exlistStr = 
    List.fold_left 
      (fun s ex -> s ^ gen_expr ex ^ ", ") "" exlist
  in
  (* get rid of the last 2 chars ', ' *)
  if exlistStr = "" then ""
  else
    String.sub exlistStr 0 ((String.length exlistStr) - 2)
    
and gen_lvalue = function
  | Variable(id) -> 
    gen_id id
  | ArrayElem(id, exlist) -> 
    gen_id id ^ "[" ^ gen_expr_list exlist ^ "]"
  

let rec gen_param = function 
  | PrimitiveDecl(typ, id) -> 
		(gen_datatype typ) ^ " " ^ (gen_id id)
  | _ -> failwith "decl list fatal error"

let rec gen_param_list paramList =
  let paramStr = 
    List.fold_left 
      (fun s param -> s ^ gen_param param ^ ", ") "" paramList
  in
  if paramStr = "" then ""
  else
    String.sub paramStr 0 ((String.length paramStr) - 2)
	
let rec gen_range id = function
	| Range(exStart, exEnd, exStep) -> 
    let exStart = gen_expr exStart in
    let exEnd = gen_expr exEnd in
    let exStep = gen_expr exStep in
    (* !!!! Needs to assign exStart, exEnd and exStep to compiled temp vars *)
    (* Shouldn't change over iteration!!! *)
    let exCmp = surr exEnd ^">"^ surr exStart ^" ? " in
    let id = gen_id id in
      "(" ^id^ "=" ^exStart^ "; " ^
      exCmp^ id^" < " ^surr exEnd^ " : " ^id^ " > " ^surr exEnd^ "; " ^
      exCmp^ id^" += " ^surr exStep^ " : " ^id^ " -= " ^surr exStep^ ")"
  | _ -> failwith "range fatal error"
	
let rec gen_iterator = function
  | RangeIterator(id, rng) -> 
    gen_range id rng
  | ArrayIterator(id, ex) -> 
    gen_id id ^ " in " ^ gen_expr ex
	
let rec gen_decl = function
  | AssigningDecl(typ, id, ex) -> 
    gen_datatype typ ^ " " ^ gen_id id ^ " = " ^ gen_expr ex
  | PrimitiveDecl(typ, id) -> 
    gen_datatype typ ^ " " ^ gen_id id


let rec eval stmts =
  match stmts with
  | [] -> ()
  | stmt :: rest ->
    begin
      match stmt with
			(* top level statements *)
      | FunctionDecl(returnTyp, funcId, paramList, stmtList) ->
        let funcId = gen_id funcId in
          begin
            print_endline @@ gen_datatype returnTyp ^ " " ^ 
              funcId ^ surr( gen_param_list paramList );
            print_endline @@ "{ // start " ^ funcId;
            eval stmtList;
            print_endline @@ "} // end " ^ funcId ^ "\n";
          end
      
      (* TODO: get rid of forward decl *)
      | ForwardDecl(returnTyp, funcId, paramList) -> 
          print_endline @@ "*forward* " ^ gen_datatype returnTyp ^ " " ^ 
            (gen_id funcId) ^ surr( gen_param_list paramList ) ^";\n";

      (* statements *)
      | IfStatement(ex, stmtIf, stmtElse) -> 
        begin
          print_endline @@ "if " ^ surr(gen_expr ex);
          print_endline "{ // start if";
          eval [stmtIf];
          print_endline "else";
          eval [stmtElse];
          print_endline "} // end if";
        end
				
      | WhileStatement(ex, stmt) -> 
        begin
          print_endline @@ "while " ^ surr(gen_expr ex);
          print_endline "{ // start while";
          eval [stmt];
          print_endline "} // end while";
        end
            
      | ForStatement(iterList, stmt) -> 
        begin
          (* for (a in 1:5, b in 7:3:-1) *)
          List.iter (fun iter -> 
            print_endline @@ "for " ^ gen_iterator iter) iterList;
          print_endline "{ // start for";
          eval [stmt];
          print_endline "} // end for";
        end
            
      | CompoundStatement(stmtList) -> 
        begin
          print_endline "{ // start compound";
          eval stmtList;
          print_endline "} // end compound";
        end

      | Declaration(dec) -> 
        print_endline @@ gen_decl dec ^ ";"
      | Expression(ex) -> 
        print_endline @@ gen_expr ex ^ ";"
      | ReturnStatement(ex) -> 
        print_endline @@ "return " ^ gen_expr ex ^ ";"
      | EmptyStatement -> 
        print_endline ";"
      | VoidReturnStatement -> 
        print_endline "return; // void"
      | BreakStatement -> 
        print_endline "break; // control"
      | ContinueStatement -> 
        print_endline "continue; // control"
      | _ -> failwith "nothing for eval()"
    end;
    eval rest

let _ =
	let lexbuf = Lexing.from_channel stdin in
	eval @@ Parser.top_level Scanner.token lexbuf
