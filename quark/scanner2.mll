{ open Parser }

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z' '_']
let sign = ['+' '-']
let floating =
    digit+ '.' digit* | '.' digit+
  | digit+ ('.' digit*)? 'e' '-'? digit+
  | '.' digit+ 'e' '-'? digit+
let complex = 
    (floating sign | sign?) floating 'i'
    
rule token = parse
  | [' ' '\t' '\r' '\n'] { token lexbuf }
  | ';' { SEMICOLON }
  | ':' { COLON }
  | ',' { COMMA }
  | '$' { DOLLAR }
  | '(' { LPAREN }  | ')' { RPAREN }
  | '{' { LCURLY }  | '}' { RCURLY }
  | '[' { LSQUARE } | ']' { RSQUARE }
  | '=' { EQUAL }
  | '?' { QUERY }

  | '+' { PLUS } | '-' { MINUS } | '*' { TIMES } | '/' { DIVIDE }
  | '%' { MODULO }
  | "<<" { LSHIFT } | ">>" { RSHIFT }
  | '<' { LT } | "<=" { LTE }
  | '>' { GT } | ">=" { GTE }
  | "==" { EQUALS } | "!=" { NOT_EQUALS }
  | '&' { BITAND } | '^' { BITXOR } | '|' { BITOR }
  | "&&" { LOGAND } | "||" { LOGOR }
  | "mod" { MODULO }

  | '!' { LOGNOT } | '~' { BITNOT }
  | "++" { INC } | "--" { DEC }
  | "**" { POWER }

  | "+=" { PLUS_EQUALS } | "-=" { MINUS_EQUALS }
  | "*=" { TIMES_EQUALS } | "/-" { DIVIDE_EQUALS }

  | sign? digit+ "/" digit+ as lit { FRACTION(lit) }

  | "true" { TRUE }
  | "false" { FALSE }

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
  | "}%"                { token lexbuf}
  | _                   { comments lexbuf}

and inline_comments = parse
  | "\n"  {token lexbuf}
  | _ {inline_comments lexbuf}

