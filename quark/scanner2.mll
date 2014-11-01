{ open Parser }

let decdigit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z' '_']
let floating =
    decdigit+ '.' decdigit* | '.' decdigit+
  | decdigit+ ('.' decdigit*)? 'e' '-'? decdigit+
  | '.' decdigit+ 'e' '-'? decdigit+

rule token = parse
  | [' ' '\t' '\r' '\n'] { token lexbuf }
  | ';' { SEMICOLON }
  | ':' { COLON }
  | '.' { DOT }
  | ',' { COMMA }
  | '$' { DOLLAR }
  | '(' { LPAREN }  | ')' { RPAREN }
  | '{' { LCURLY }  | '}' { RCURLY }
  | '[' { LSQUARE } | ']' { RSQUARE }
  | '=' { EQUAL }

  | '+' { PLUS } | '-' { MINUS } | '*' { TIMES } | '/' { DIVIDE }
  | '%' { MODULO }
  | "<<" { LSHIFT } | ">>" { RSHIFT }
  | '<' { LT } | "<=" { LTE }
  | '>' { GT } | ">=" { GTE }
  | "==" { EE } | "!=" { NE }
  | '&' { BITAND } | '^' { BITXOR } | '|' { BITOR }
  | "&&" { LOGAND } | "||" { LOGOR }

  | '!' { LOGNOT } | '~' { BITNOT }
  | "++" { INC } | "--" { DEC }

  | "+=" { PLUS_EQUALS } | "-=" { MINUS_EQUALS }
  | "*=" { TIMES_EQUALS } | "/-" { DIVIDE_EQUALS }
  | "%=" { MODULO_EQUALS }

  | "bool" | "int" | "float" | "complex" | "void" | "string" | "list"
      as primitive { TYPE(primitive) }

  | "return" { RETURN }
  | "if" { IF }
  | "else" { ELSE }
  | "for" { FOR }
  | "while" { WHILE }
  | "in" { IN }

  | letter (letter | decdigit)* as lit { ID(lit) }

  | "%{" { comments lexbuf }
  | "%" {inline_comments lexbuf}

  | eof { EOF }

and comments = parse
  | "}%"                { token lexbuf}
  | _                   { comments lexbuf}

and inline_comments = parse
  | "\n"  {token lexbuf}
  | _ {inline_comments lexbuf}

