{ open Parser }

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z' '_']
let sign = ['+' '-']
let floating =
    digit+ '.' digit* | '.' digit+
  | digit+ ('.' digit*)? 'e' '-'? digit+
  | '.' digit+ 'e' '-'? digit+
        
rule token = parse
  (* whitespace *)
  | [' ' '\t' '\r' '\n'] { token lexbuf }

  (* meaningful character sequences *)
  | ';' { SEMICOLON }
  | ':' { COLON }
  | ',' { COMMA }
  | '$' { DOLLAR }
  | '(' { LPAREN }  | ')' { RPAREN }
  | '{' { LCURLY }  | '}' { RCURLY }
  | '[' { LSQUARE } | ']' { RSQUARE }
  | '=' { ASSIGN }
  | ''' { PRIME }
  | '?' { QUERY }
  | 'i' { COMPLEX_SYM }
  | "<#" { LQREG }
  | "#>" { RQREG }
  | "def" { DEF }


  (* arithmetic *)
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { TIMES }
  | '/' { DIVIDE }
  | "mod" { MODULO }

  (* logical *)
  | '<'     { LT }
  | "<="    { LTE }
  | '>'     { GT }
  | ">="    { GTE }
  | "=="    { EQUALS }
  | "!="    { NOT_EQUALS }
  | "and"   { AND }
  | "or"    { OR }
  | '!'     { NOT }
  | "**"    { POWER }

  (* unary *)
  | '~'     { BITNOT }
  | '&'     { BITAND }
  | '^'     { BITXOR }
  | '|'     { BITOR }
  | "<<"    { LSHIFT }
  | ">>"    { RSHIFT }

  (* special assignment *)
  | "+=" { PLUS_EQUALS }
  | "-=" { MINUS_EQUALS }
  | "*=" { TIMES_EQUALS }
  | "/=" { DIVIDE_EQUALS }
  | "++" { INCREMENT }
  | "--" { DECREMENT }

  | "bool"      { BOOLEAN }
  | "string"    { STRING }
  | "int"       { INT }
  | "float"     { FLOAT }
  | "void"      { VOID }
  | "complex"   { COMPLEX }
  | "fraction"  { FRACTION }
  | "qreg"      { QREG }

  (* literals *) 
  | digit+ as lit { INT_LITERAL(int_of_string lit) } 
  | floating as lit { FLOAT_LITERAL(float_of_string lit) }
  | "true" as lit { BOOLEAN_LITERAL(bool_of_string lit) }
  | "false" as lit { BOOLEAN_LITERAL(bool_of_string lit) }
  | '"' (('\\' _ | [^ '"'])* as str) '"' { STRING_LITERAL(str) }

  (* datatypes
  | "bool" 
  | "int" 
  | "float" 
  | "complex" 
  | "void" 
  | "string" 
  | "array"
      as primitive { TYPE(primitive) }
  *)

  (* keywords *)
  | "return" { RETURN }
  | "if" { IF }
  | "else" { ELSE }
  | "for" { FOR }
  | "while" { WHILE }
  | "in" { IN }

  (* ID *)
  | letter (letter | digit)* as lit { ID(lit) }

  (* comments *)
  | "%{" { comments lexbuf }
  | "%" {inline_comments lexbuf}

  (* end of file *)
  | eof { EOF }

and comments = parse
  | "}%" { token lexbuf}
  | _ { comments lexbuf}

and inline_comments = parse
  | "\n" {token lexbuf}
  | _ {inline_comments lexbuf}

