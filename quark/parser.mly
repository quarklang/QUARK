%{ open Ast;; open Type %}

%token LPAREN RPAREN LCURLY RCURLY LSQUARE RSQUARE
%token LQREG RQREG
%token COMMA SEMICOLON COLON
%token ASSIGN
%token PLUS_EQUALS MINUS_EQUALS TIMES_EQUALS DIVIDE_EQUALS MODULO_EQUALS
%token LSHIFT_EQUALS RSHIFT_EQUALS BITOR_EQUALS BITAND_EQUALS BITXOR_EQUALS
%token LSHIFT RSHIFT BITAND BITOR BITXOR AND OR
%token LT LTE GT GTE EQUALS NOT_EQUALS
%token PLUS MINUS TIMES DIVIDE MODULO
%token NOT UMINUS BITNOT DECREMENT INCREMENT
%token DOLLAR PRIME QUERY POWER COMPLEX_SYM
%token IF ELSE WHILE FOR IN
%token COMPLEX_LITERAL FRACTION_LITERAL
%token DEF
%token RETURN
%token EOF
%token BOOLEAN STRING INT FLOAT QREG FRACTION COMPLEX VOID
%token <string> ID TYPE STRING_LITERAL INT_LITERAL FLOAT_LITERAL BOOLEAN_LITERAL

%right ASSIGN PLUS_EQUALS MINUS_EQUALS TIMES_EQUALS DIVIDE_EQUALS MODULO_EQUALS

%left DOLLAR
%left FRACTION
%left COMPLEX_SYM
%left OR
%left AND
%left BITOR
%left BITXOR
%left BITAND
%left EQUALS NOT_EQUALS
%left LT LTE GT GTE
%left LSHIFT RSHIFT
%left PLUS MINUS
%left TIMES DIVIDE MODULO
%left DEF

%right NOT BITNOT POWER UMINUS

%nonassoc IFX
%nonassoc ELSE

%start top_level
%type <Ast.statement list> top_level

%%

ident:
    ID { Ident($1) }

var_type:
    INT      { Int }
  | FLOAT    { Float }
  | BOOLEAN  { Bool }
  | STRING   { String }
  | QREG     { QReg }
  | FRACTION { Fraction }
  | COMPLEX  { Complex }
  | VOID     { Void }

datatype:
    var_type { DataType($1) }
  | var_type LSQUARE RSQUARE { ArrayType(DataType($1)) }
  | datatype LSQUARE RSQUARE { ArrayType($1) }

/* Variables that can be assigned a value */
lvalue:
  | ident { Variable($1) }
  | ident LSQUARE expr_list RSQUARE { ArrayElem($1, $3) }

expr:
  /* Logical */
  | expr LT expr          { Binop($1, Less, $3) }
  | expr LTE expr         { Binop($1, LessEq, $3) }
  | expr GT expr          { Binop($1, Greater, $3) }
  | expr GTE expr         { Binop($1, GreaterEq, $3) }
  | expr EQUALS expr      { Binop($1, Eq, $3) }
  | expr NOT_EQUALS expr  { Binop($1, NotEq, $3) }
  | expr AND expr         { Binop($1, And, $3) }
  | expr OR expr          { Binop($1, Or, $3) }
  
  /* Unary */
  | BITNOT expr             { Unop(BitNot, $2) }
  | MINUS expr %prec UMINUS { Unop(Neg, $2) }
  | NOT expr                { Unop(Not, $2) }

  /* Arithmetic */
  | expr PLUS expr    { Binop($1, Add, $3) }
  | expr MINUS expr   { Binop($1, Sub, $3) }
  | expr TIMES expr   { Binop($1, Mul, $3) }
  | expr DIVIDE expr  { Binop($1, Div, $3) }
  | expr MODULO expr  { Binop($1, Mod, $3) }
  | expr POWER expr  { Binop($1, Pow, $3) }

  /* Bitwise */
  | expr BITAND expr        { Binop($1, BitAnd, $3) }
  | expr BITXOR expr        { Binop($1, BitXor, $3) }
  | expr BITOR expr         { Binop($1, BitOr, $3) }
  | expr LSHIFT expr        { Binop($1, Lshift, $3) }
  | expr RSHIFT expr        { Binop($1, Rshift, $3) }

  | LPAREN expr RPAREN { $2 }

  /* Assignment */
  | lvalue ASSIGN expr { Assign($1, $3) }
  | lvalue             { Lval($1) }

	/* Special assignment */
	| lvalue PLUS_EQUALS expr { AssignOp($1, AddEq, $3) }
	| lvalue MINUS_EQUALS expr { AssignOp($1, SubEq, $3) } 
	| lvalue TIMES_EQUALS expr { AssignOp($1, MulEq, $3) }
	| lvalue DIVIDE_EQUALS expr { AssignOp($1, DivEq, $3) }

	/* Post operation */
	| lvalue INCREMENT { PostOp($1, Inc) }
	| lvalue DECREMENT { PostOp($1, Dec) }

  /* literals */
  | INT_LITERAL                                     { IntLit($1) }
  | FLOAT_LITERAL                                   { FloatLit($1) }
  | BOOLEAN_LITERAL                                 { BoolLit($1) }
  | expr DOLLAR expr                                { FractionLit($1, $3) }
  | STRING_LITERAL                                  { StringLit($1) }
  | LSQUARE expr_list RSQUARE                       { ArrayLit($2) }
  | COMPLEX_SYM expr COMMA expr RPAREN              { ComplexLit($2, $4) }
  | LQREG expr COMMA expr RQREG       { QRegLit($2, $4) }

  /* function call */
  | ident LPAREN RPAREN             { FunctionCall($1, []) }
  | ident LPAREN expr_list RPAREN   { FunctionCall ($1, $3) }

expr_list:
  | expr COMMA expr_list { $1 :: $3 }
  | expr                 { [$1] }

decl:
  | datatype ident ASSIGN expr SEMICOLON                { AssigningDecl($1, $2, $4) }
  | datatype ident SEMICOLON                            { PrimitiveDecl($1, $2) }

statement:
  | IF LPAREN expr RPAREN statement ELSE statement
      { IfStatement($3, $5, $7) }
  | IF LPAREN expr RPAREN statement %prec IFX
      { IfStatement($3, $5, EmptyStatement) }

  | WHILE LPAREN expr RPAREN statement { WhileStatement($3, $5) }
  | FOR LPAREN iterator_list RPAREN statement { ForStatement($3, $5) }

  | LCURLY statement_seq RCURLY { CompoundStatement($2) }

  | expr SEMICOLON { Expression($1) }
  | SEMICOLON { EmptyStatement }
  | decl { Declaration($1) }

  | RETURN expr SEMICOLON { ReturnStatement($2) }
  | RETURN SEMICOLON { VoidReturnStatement }


iterator_list:
  | iterator COMMA iterator_list { $1 :: $3 }
  | iterator { [$1] }

iterator:
  | ident IN range { RangeIterator($1, $3) }
  | ident IN expr { ArrayIterator($1, $3) }

range:
  | expr COLON expr COLON expr { Range($1, $3, $5) }
  | expr COLON expr { Range($1, $3, IntLit("1")) }
  | COLON expr COLON expr { Range(IntLit("0"), $2, $4) }
  | COLON expr { Range(IntLit("0"), $2, IntLit("1")) }

top_level_statement:
  | DEF datatype ident LPAREN param_list RPAREN LCURLY statement_seq RCURLY
      { FunctionDecl($2, $3, $5, $8) }
  | datatype ident LPAREN param_list RPAREN SEMICOLON
      { ForwardDecl($1, $2, $4) }
  | decl { Declaration($1) }

param:
  | datatype ident { PrimitiveDecl($1, $2) }

non_empty_param_list:
  | param COMMA non_empty_param_list { $1 :: $3 }
  | param { [$1] }

param_list:
  | non_empty_param_list { $1 }
  | { [] }

top_level:
  | top_level_statement top_level {$1 :: $2}
  | top_level_statement { [$1] }

statement_seq:
  | statement statement_seq {$1 :: $2 }
  | { [] }

%%

