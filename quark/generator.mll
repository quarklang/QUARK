open Ast
open Sast
open Printf
open Pretty_c
open Type

let print = "print"
let get_time = "print_time"
let prefix_array = "a_"
let prefix_global_var = "u_"
let prefix_event = "event_"
let prefix_event_list = "event_q_"
let code_event_base = "struct " ^ prefix_event^
  " {\n\tunsigned int time;\n\tunsigned int inc_time;\n\tstd::string name;\n\t" ^
  "virtual unsigned int get_time() {};\n\t" ^
  "virtual unsigned int get_inc_time() {};\n\t" ^
  "virtual void (set_time)(unsigned int time_) {};\n\t" ^
  "virtual std::string get_name() {};\n\t" ^
  "virtual void foo() {};\n\tvirtual ~"^ prefix_event ^ "() {};\n};\n"
let code_event_list = "struct "^ prefix_event_list ^
  " {\n\tbool empty() {return event_q.empty();}\n\t" ^
  "unsigned int get_time() {return global_time;}\n\t" ^
  prefix_event ^ "* pop() {\n\t\t" ^
  prefix_event ^ " *front = event_q.front();\n\t\t" ^
  "global_time = front->get_time();\n\t\t" ^
  "event_q.pop_front();\n\t\treturn front;\n\t}\n\t" ^
  "void add(unsigned int time_, " ^ prefix_event ^
  " *obj_) {\n\t\tbool eol = true;\n\t\tstd::deque<" ^
  prefix_event ^ "*>::iterator it;\n\t\tif (obj_ == NULL)\n\t\t\treturn;" ^
  "\n\t\tfor (it = event_q.begin(); it != event_q.end(); it++) " ^
  "{\n\t\t\tif ((*it)->get_time() > time_) {\n\t\t\t\t" ^
  "event_q.insert(it, obj_);\n\t\t\t\teol = false;\n\t\t\t\tbreak;" ^
  "\n\t\t\t}\n\t\t}\n\t\tif (eol)\n\t\t\tevent_q.push_back(obj_);" ^
  "\n\t}\n\tprivate:\n\t\tunsigned int global_time;\n\t\tstd::deque<" ^
  prefix_event ^ "*> event_q;\n};\n" ^ prefix_event_list ^ 
  " event_q;\n\n"
let code_directives = "#include <iostream>\n#include <string>\n#include <deque>\n#include <vector>\n#include <cstdlib>\n"
let code_event_list_do = "while(!event_q.empty()) {\n\tevent_q.pop()->foo();\n\t}\n"
let header = code_directives^code_event_base^code_event_list

let gen_id = function
  Ident(id) -> id

let gen_sid_prefix scope lcl_prefix = match scope with
  Global -> prefix_global_var
| Local -> lcl_prefix

let gen_sid sident lcl_prefix = match sident with
  SIdent(sid, scope) -> gen_sid_prefix scope lcl_prefix ^ gen_id sid

let gen_plain_sid sident = match sident with
  SIdent(sid, scope) -> gen_id sid

let gen_name = function
  Time_struct_name(s) -> s

let gen_link = function
  Link(s) -> s

let gen_unop = function
  Neg -> "-"
| Inc -> "++"
| Dec -> "--"
| Not -> "!"

let gen_binop = function
  Add -> "+"
| Sub -> "-"
| Mult -> "*"
| Div -> "/"
| Equal -> "=="
| Neq -> "!="
| Less -> "<"
| Leq -> "<="
| Greater -> ">"
| Geq -> ">="
| Mod -> "%"
| And -> "&&"
| Or -> "||"

let gen_var_type = function
  Int -> "int"
| Float -> "float"
| String -> "std::string"
| Boolean -> "bool"
| Void -> "void"

let gen_plain_var_type = function
  Int -> "int"
| Float -> "float"
| String -> "char *"
| Boolean -> "bool"
| Void -> "void"

let rec gen_datatype = function
  Datatype(var_type) -> gen_var_type var_type
| Arraytype(datatype) -> "std::vector<" ^ gen_datatype datatype ^ ">"

let rec gen_plain_datatype = function
  Datatype(var_type) -> gen_plain_var_type var_type
| Arraytype(datatype) -> gen_plain_datatype datatype

let rec gen_formal formal prefix = match formal with
  Formal(datatype, id) -> gen_datatype datatype ^ " " ^ prefix ^ gen_id id

let rec gen_sexpr sexpr lcl_prefix = match sexpr with
  SIntLit(i, d) -> string_of_int i
| SBoolLit(b, d) -> string_of_bool b
| SFloatLit(f, d) -> string_of_float f
| SStringLit(s, d) -> "\"" ^ s ^ "\""
| SVariable(sident, d) -> gen_sid sident lcl_prefix
| SUnop(unop, sexpr, d) -> gen_unop unop ^ "(" ^ gen_sexpr sexpr lcl_prefix ^ ")"
| SBinop(sexpr1, binop, sexpr2, d) -> gen_sexpr sexpr1 lcl_prefix ^ gen_binop binop ^
    gen_sexpr sexpr2 lcl_prefix
| SArrElem(sident, i, d) -> gen_sid sident lcl_prefix^ "[" ^ gen_sexpr i lcl_prefix ^ "]"
| SExprAssign(sident, sexpr, d) -> gen_sid sident lcl_prefix^ " = " ^
    gen_sexpr sexpr lcl_prefix
| SCast(datatype, sexpr, d) -> "(" ^ gen_datatype datatype^ ") " ^
    gen_sexpr sexpr lcl_prefix
| SCall(sident, sexpr_list, d) -> if ((gen_plain_sid sident) = print)
    then "std::cout << ("^ gen_sexpr_list sexpr_list lcl_prefix ^ ") << std::endl"
    else begin
      if ((gen_plain_sid sident) = get_time) then 
        "std::cout << \"Time now: \" <<event_q.get_time() << std::endl"
      else
        gen_sid sident lcl_prefix ^ "(" ^ gen_sexpr_list sexpr_list lcl_prefix ^ ")"
    end
and gen_expr expr prefix = match expr with
  IntLit(i) -> string_of_int i
| BoolLit(b) -> string_of_bool b
| FloatLit(f) -> string_of_float f
| StringLit(s) -> "\"" ^ s ^ "\""
| Variable(ident) -> prefix ^ gen_id ident
| Unop(unop, expr) -> gen_unop unop ^ "(" ^ gen_expr expr prefix ^ ")"
| Binop(expr1, binop, expr2) -> gen_expr expr1 prefix ^ gen_binop binop ^
    gen_expr expr2 prefix
| ArrElem(ident, i) -> prefix ^ gen_id ident ^ "[" ^ gen_expr i prefix^ "]"
| ExprAssign(ident, expr) -> prefix ^ gen_id ident ^ " = " ^ gen_expr expr prefix
| Cast(datatype, expr) -> "(" ^ gen_datatype datatype^ ") " ^ gen_expr expr prefix
| Call(ident, expr_list) -> if ((gen_id ident) = print)
    then "std::cout << ("^ gen_expr_list expr_list prefix ^ ") << std::endl"
    else prefix ^ gen_id ident ^ "(" ^ gen_expr_list expr_list prefix ^ ")"

and gen_sstmt sstmt lcl_prefix = match sstmt with
  SBlock(sstmt_list) -> "{\n\t" ^ gen_sstmt_list sstmt_list  lcl_prefix ^ "\n\t}\n\t"
| SSExpr(sexpr) -> gen_sexpr sexpr lcl_prefix ^ ";\n\t"
| SReturn(sexpr) -> "return " ^ gen_sexpr sexpr lcl_prefix ^ ";\n\t"
| SIf(sexpr, sstmt1, sstmt2) -> "if (" ^ gen_sexpr sexpr lcl_prefix ^
   ")\n\t" ^ gen_sstmt sstmt1 lcl_prefix ^ "\n\telse " ^ gen_sstmt sstmt2 lcl_prefix
| SFor(sexpr1, sexpr2, sexpr3, sstmt) ->"for (" ^ gen_sexpr sexpr1 lcl_prefix ^
   "; "^ gen_sexpr sexpr2 lcl_prefix ^ "; " ^ gen_sexpr sexpr3 lcl_prefix ^ ")\n" ^
   gen_sstmt sstmt lcl_prefix
| SWhile(sexpr, sstmt) -> "while (" ^ gen_sexpr sexpr lcl_prefix ^
   ")\n" ^ gen_sstmt sstmt lcl_prefix ^ ";\n\t"
| SDeclaration(sdecl) -> gen_sdecl sdecl lcl_prefix ^ ";\n\t"
| SAssign(sident, sexpr) -> gen_sid sident lcl_prefix ^ " = " ^
   gen_sexpr sexpr lcl_prefix ^ ";\n\t"
| SArrAssign(sident, sexpr_list) -> gen_sid sident lcl_prefix ^ ".clear();\n\t" ^
     (gen_array_sexpr_list sexpr_list sident lcl_prefix) ^ ";\n\t"
| SArrElemAssign(sident, i, sexpr) -> gen_sid sident lcl_prefix ^
    "[" ^ gen_sexpr i lcl_prefix ^ "] = " ^ gen_sexpr sexpr lcl_prefix ^ ";\n\t"
| STerminate -> "exit(0);\n\t"

(*semicolon and newline handled in gen_stmt because blocks dont have semicolon*)
and gen_stmt stmt prefix = match stmt with
  Block(stmt_list) -> "{\n\t" ^ gen_stmt_list stmt_list prefix ^ "\n\t}\n\t"
| Expr(expr) -> gen_expr expr prefix ^ ";\n\t"
| Return(expr) -> "return " ^ gen_expr expr prefix ^ ";\n\t"
| If(expr, stmt1, stmt2) -> "if (" ^ gen_expr expr prefix ^ ")\n\t" ^ 
    gen_stmt stmt1 prefix ^ "\n\telse " ^ gen_stmt stmt2 prefix
| For(expr1, expr2, expr3, stmt) -> "for (" ^ gen_expr expr1 prefix ^ "; " ^ 
    gen_expr expr2 prefix ^ "; " ^ gen_expr expr3 prefix ^ ")\n" ^
    gen_stmt stmt prefix
| While(expr, stmt) -> "while (" ^ gen_expr expr prefix ^ ")\n" ^
    gen_stmt stmt prefix ^ ";\n\t"
| Declaration(decl) -> gen_decl decl prefix ^ ";\n\t"
| Assign(ident, expr) -> prefix ^ gen_id ident ^ " = " ^ gen_expr expr prefix ^ ";\n\t"
| ArrAssign(ident, expr_list) -> prefix ^ gen_id ident ^ ".clear();\n\t" ^
     (gen_array_expr_list expr_list ident prefix) ^ ";\n\t"
| ArrElemAssign(ident, i, expr) -> prefix ^ gen_id ident ^
    "[" ^ gen_expr i prefix ^ "] = " ^ gen_expr expr prefix ^ ";\n\t"
| Terminate -> "exit(0);\n\t"

(*gen_sdecl only appears within time blocks, VarDecls are ignored*)
and gen_sdecl sdecl lcl_prefix = match sdecl with
  SVarDecl(datatype, sid) -> ""
| SVarAssignDecl(datatype, sident, svalue) -> gen_svalue datatype svalue sident lcl_prefix

(*gen_svalue only appears within time blocks declartions, assume all local*)
and gen_svalue datatype svalue sident lcl_prefix = match svalue with
  SExprVal(sexpr) -> lcl_prefix ^ gen_plain_sid sident ^
    " = " ^ gen_sexpr sexpr lcl_prefix ^ ";\n"
| SArrVal(sexpr_list) -> gen_sid sident lcl_prefix ^ ".clear();\n" ^
     (gen_array_sexpr_list sexpr_list sident lcl_prefix) ^ ";\n"

(*semicolon and newline handled in gen_decl since array decl assignment is actually vector push_back*)
and gen_decl decl prefix = match decl with
  VarDecl(datatype, id) -> gen_datatype datatype ^ " " ^ prefix ^ gen_id id  ^ ";\n"
| VarAssignDecl(datatype, ident, value) -> gen_value datatype value ident prefix

and gen_value datatype value ident prefix = match value with
  ExprVal(expr) -> gen_datatype datatype ^ " " ^ prefix ^ gen_id ident ^ " = " ^ gen_expr expr prefix ^ ";\n"
| ArrVal(expr_list) -> "const " ^ gen_plain_datatype datatype ^ " " ^
    prefix_array ^ gen_id ident ^ "[] = {"^ gen_expr_list expr_list prefix ^ "};\n" ^
    gen_datatype datatype ^ prefix ^ gen_id ident ^"( " ^ prefix_array ^ gen_id ident ^ ", " ^
    prefix_array ^ gen_id ident ^ "+sizeof(" ^ prefix_array ^ gen_id ident ^
    ")/sizeof(" ^ prefix_array ^ gen_id ident ^ "[0]) );\n"

and gen_array_sexpr_list sexpr_list sident lcl_prefix = match sexpr_list with
 [] -> ""
| h::[] -> gen_sid sident lcl_prefix ^ ".push_back(" ^ gen_sexpr h lcl_prefix ^");\n"
| h::t -> gen_sid sident lcl_prefix ^ ".push_back(" ^ gen_sexpr h lcl_prefix
  ^ ");\n" ^ (gen_array_sexpr_list t sident lcl_prefix)

and gen_array_expr_list expr_list ident prefix = match expr_list with
 [] -> ""
| h::[] -> prefix ^ gen_id ident ^ ".push_back(" ^ gen_expr h prefix ^");\n"
| h::t -> prefix ^ gen_id ident ^ ".push_back(" ^ gen_expr h prefix
  ^ ");\n" ^ (gen_array_expr_list t ident prefix)

and gen_func func prefix =
  gen_datatype func.return ^ " " ^ prefix ^ gen_id func.fname ^
  "(" ^ gen_formal_list func.formals prefix ^ 
  ") {\n" ^ gen_stmt_list func.body prefix ^ "}\n"

and gen_decl_list decl_list prefix = match decl_list with
 [] -> ""
| h::[] -> gen_decl h prefix
| h::t -> gen_decl h prefix ^ gen_decl_list t prefix

and gen_func_list func_list prefix = match func_list with
 [] -> ""
| h::[] -> gen_func h prefix
| h::t -> gen_func h prefix ^ gen_func_list t prefix

and gen_formal_list formal_list prefix = match formal_list with
 [] -> ""
| h::[] -> gen_formal h prefix
| h::t -> gen_formal h prefix ^ ", " ^ gen_formal_list t prefix

and gen_sstmt_list sstmt_list  lcl_prefix = match sstmt_list with
 [] -> ""
| h::[] -> gen_sstmt h lcl_prefix
| h::t -> gen_sstmt h lcl_prefix ^ gen_sstmt_list t lcl_prefix

and gen_stmt_list stmt_list prefix = match stmt_list with
 [] -> ""
| h::[] -> gen_stmt h prefix
| h::t -> gen_stmt h prefix ^ gen_stmt_list t prefix

and gen_sexpr_list sexpr_list lcl_prefix = match sexpr_list with
 [] -> ""
| h::[] -> gen_sexpr h lcl_prefix
| h::t -> gen_sexpr h lcl_prefix ^ ", " ^ gen_sexpr_list t lcl_prefix

and gen_expr_list expr_list prefix = match expr_list with
 [] -> ""
| h::[] -> gen_expr h prefix
| h::t -> gen_expr h prefix ^ ", " ^ gen_expr_list t prefix

let gen_time_block_header link =
  "unsigned int " ^ link ^ "_time = 0;\nstruct " ^ link ^
  "_link_ : public event_ {\n\tvirtual void set_next(" ^ link ^
  "_link_ *n){};\n};\nstd::vector<" ^ link ^ "_link_*> " ^ link ^ "_list;\n"

let rec gen_struct = function
  Time_struct(name, i, link, sstmt_list) -> "struct " ^ gen_name name ^
    " : public " ^ gen_link link ^ "_link_ {\n\tunsigned int time;\n\t" ^
    "unsigned int inc_time;\n\tstd::string name;\n\t" ^ 
    gen_name name ^ "() : inc_time(" ^
    string_of_int i ^ ") "^ ", time(" ^ string_of_int i ^
    "), name(\"" ^ gen_name name ^ 
    "\") {}\n\tunsigned int get_time() {return time;}\n\t" ^
    "unsigned int get_inc_time() {return inc_time;}\n\t" ^ 
    "void set_time(unsigned int time_) {time = time_;}\n\t" ^
    "std::string get_name() {return name;}\n\t" ^ gen_link link ^
    "_link_ *next;\n\tvoid set_next(" ^ gen_link link ^
    "_link_ *n) {next = n;};\n\t" ^ "void foo() {\n\t" ^
    gen_sstmt_list sstmt_list (gen_link link) ^ "\n\tif(next != NULL) {\n\t\t" ^
    gen_link link ^ "_time += next->get_inc_time();\n\t\t" ^
    "next->set_time(" ^ gen_link link ^ "_time);\n\t\t" ^
    "event_q.add(" ^ gen_link link ^ "_time, next);\n\t\t}\n\t}\n};"

and gen_struct_list struct_list = match struct_list with
 [] -> ""
| h::[] -> gen_struct h ^ "\n"
| h::t -> gen_struct h ^ "\n" ^ gen_struct_list t

let rec gen_time_block = function
  Time_block(link, decl_list, struct_list) ->
  gen_decl_list decl_list (gen_link link) ^
  gen_time_block_header (gen_link link) ^
  gen_struct_list struct_list 

and gen_time_block_list = function
 [] -> ""
| h::[] -> gen_time_block h
| h::t -> gen_time_block h  ^ "\n" ^ gen_time_block_list t

let gen_struct_obj = function
  Time_struct_obj(name, link) -> gen_name name ^ " " ^ gen_name name ^ "obj;\n\t" ^
    gen_link link ^ "_list.push_back(&" ^ gen_name name ^ "obj);\n\t"

let gen_init_linker = function
  Link(s) -> "for (int i = 0; i < " ^ s ^ "_list.size(); i++)\n\t" ^
    "{\n\t\tif (i != " ^ s ^ "_list.size()-1)\n\t\t\t" ^ 
    s ^ "_list[i]->set_next(" ^ s ^ "_list[i+1]);\n\t\telse\n\t\t\t" ^
    s ^ "_list[i]->set_next(NULL);\n\t}\n\t" ^
    "event_q.add(" ^ s ^ "_block_0obj.get_time(), &" ^ s ^ "_block_0obj);\n\t"

let gen_always_linker = function
  Link(s) -> "for (int i = 0; i < " ^ s ^ "_list.size(); i++)\n\t" ^
    "{\n\t\tif (i != " ^ s ^ "_list.size()-1)\n\t\t\t" ^ 
    s ^ "_list[i]->set_next(" ^ s ^ "_list[i+1]);\n\t\telse\n\t\t\t" ^
    s ^ "_list[i]->set_next(" ^ s ^ "_list[0]);\n\t}\n\t" ^
    "event_q.add(" ^ s ^ "_block_0obj.get_time(), &"^ s ^"_block_0obj);\n\t"

let rec gen_init_linker_list = function
 [] -> ""
| h::[] -> gen_init_linker h
| h::t -> gen_init_linker h ^ gen_init_linker_list t

let rec gen_always_linker_list = function
 [] -> ""
| h::[] -> gen_always_linker h
| h::t -> gen_always_linker h ^ gen_always_linker_list t

let rec gen_struct_obj_list = function
 [] -> ""
| h::[] -> gen_struct_obj h
| h::t -> gen_struct_obj h ^ gen_struct_obj_list t

let rec gen_event_q_add_list = function
[] -> ""
| h::[] -> gen_event_q_add h
| h::t -> gen_event_q_add h ^ gen_event_q_add_list t

and gen_event_q_add = function
  Time_struct_obj(name, link) -> "event_q.add("^ gen_name name ^
    "obj.get_time(), &" ^ gen_name name ^ "obj);\n\t" 

(*all arguments are lists*)
let gen_main = function
  Main(time_block_obj_l, init_link_l, always_link_l) ->
  gen_struct_obj_list time_block_obj_l ^ 
  gen_init_linker_list init_link_l ^
  gen_always_linker_list always_link_l

let gen_program = function
  Pretty_c(global_decl_list, global_func_list, time_block_list, main) ->
  header ^
  gen_func_list global_func_list prefix_global_var ^
  gen_decl_list global_decl_list prefix_global_var ^
  gen_time_block_list time_block_list ^ "\nint main() {\n\t" ^
  gen_main main ^
  code_event_list_do ^ "return 0;\n}"
