{ open Parser }

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z' '_']
let sign = ['+' '-']
let floating =
    digit+ '.' digit* | '.' digit+
  | digit+ ('.' digit*)? 'e' '-'? digit+
  | '.' digit+ 'e' '-'? digit+
        
rule token = parse
  | [' ' '\t' '\r' '\n'] { token lexbuf }
  | ';' { SEMICOLON }
  | ':' { COLON }
  | ',' { COMMA }
  | '$' { DOLLAR }
  | '(' { LPAREN }  | ')' { RPAREN }
  | '{' { LCURLY }  | '}' { RCURLY }
  | '[' { LSQUARE } | ']' { RSQUARE }
  | '=' { EQUAL_TO }
  | ''' { PRIME }
  | '?' { QUERY }

  | '+' { PLUS } | '-' { MINUS } | '*' { TIMES } | '/' { DIVIDE }
  | "mod" { MODULO }
  | "<<" { LSHIFT } | ">>" { RSHIFT }
  | '<' { LESS_THAN } | "<=" { LESS_THAN_EQUAL }
  | '>' { GREATER_THAN } | ">=" { GREATER_THAN_EQUAL }
  | "==" { EQUALS } | "!=" { NOT_EQUALS }
  | '&' { BITAND } | '^' { BITXOR } | '|' { BITOR }
  | "&&" { LOGAND } | "||" { LOGOR }

  | '!' { LOGNOT } | '~' { BITNOT }
  | "++" { INCREMENT } | "--" { DECREMENT }
  | "**" { POWER }

  | "+=" { PLUS_EQUALS } | "-=" { MINUS_EQUALS }
  | "*=" { TIMES_EQUALS } | "/-" { DIVIDE_EQUALS }

  | sign? digit+ as lit { INT(lit) } 
  | floating as lit { FLOAT(lit) }
  | sign? digit+ "/" digit+ as lit { FRACTION(lit) }
  | (floating sign | sign?) floating 'i' { COMPLEX(lit) }

  | "true" { TRUE }
  | "false" { FALSE }

  | "<#" { LQREGISTER }
  | "#>" { RQREGISTER }

  | '"' (('\\' _ | [^ '"'])* as str) '"' { STRING(str) }

  | "bool" | "int" | "float" | "complex" | "void" | "string" | "list"
      as primitive { TYPE(primitive) }

  | "return" { RETURN }
  | "if" { IF }
  | "else" { ELSE }
  | "for" { FOR }
  | "while" { WHILE }
  | "in" { IN }

  | letter (letter | digit)* as lit { ID(lit) }

  | "%{" { comments lexbuf }
  | "%" {inline_comments lexbuf}

  | eof { EOF }

and comments = parse
  | "}%" { token lexbuf}
  | _ { comments lexbuf}

and inline_comments = parse
  | "\n" {token lexbuf}
  | _ {inline_comments lexbuf}

