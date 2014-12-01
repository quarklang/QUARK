open Ast
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
| AndEq -> "&="
| _ -> failwith "unhandled binop"

let gen_vartype = function
  | Int -> "int64_t"
  | Float -> "float"
  | Bool -> "bool"
  | Fraction -> "Frac"
  | Complex -> "complex<float>"
  | QReg -> "Qureg"
  | String -> "string"
  | Void -> "void"

let rec gen_datatype = function
	| DataType(t) -> 
    gen_vartype t
	| ArrayType(t) -> 
		gen_datatype t ^ "[]"
	| MatrixType(t) -> 
   (match t with
    | DataType(matType) -> 
      (match matType with
      (* only support 3 numerical types *)
      | Int | Float | Complex -> 
      "Matrix<" ^ gen_vartype matType ^ ", Dynamic, Dynamic>"
      | _ -> failwith "Non-numerical matrix type")
    (* we shouldn't support float[][[]] *)
    | _ -> 
      failwith "Bad matrix type")


let rec gen_expr = function
  (* literals *)
  | IntLit(x) | FloatLit(x) | BoolLit(x) -> 
    x
  | StringLit(s) -> 
    "\"" ^ s ^ "\""

  | ArrayLit(exlist) -> 
    "[" ^ gen_expr_list exlist ^ "]"

  | MatrixLit(exlistlist) -> 
    "Matrix<>(" ^ gen_matrix_list exlistlist ^ ")"

  | FractionLit(exNum, exDenom) -> 
    "Frac(" ^ gen_expr exNum ^ ", " ^ gen_expr exDenom ^ ")"

  | QRegLit(ex1, ex2) -> 
    "Qureg::create<true>(" ^ gen_expr ex1 ^ ", " ^ gen_expr ex2 ^ ")"

  | ComplexLit(exReal, exImag) -> 
    "complex<float>(" ^ gen_expr exReal ^ ", " ^ gen_expr exImag ^ ")"
  
  (* Binary ops *)
  (* '+' used for matrix addition, '&' for array concatenation *)
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

and gen_matrix_list exlistlist =
  let exlistlistStr = 
    List.fold_left 
      (fun s exlist -> s ^ gen_expr_list exlist ^ "; ") "" exlistlist
  in
  (* get rid of the last 2 chars ', ' *)
  if exlistlistStr = "" then ""
  else
    String.sub exlistlistStr 0 ((String.length exlistlistStr) - 2)
  

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
            
      | ForStatement(iter, stmt) -> 
        begin
          (* for (a in 1:5, b in 7:3:-1) *)
          (* List.iter (fun iter -> 
            print_endline @@ "for " ^ gen_iterator iter) iterList; *)
          print_endline @@ "for " ^ gen_iterator iter;
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
