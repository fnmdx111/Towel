%{ open Ast %}

%token IFGEZ IFGZ IFLEZ IFLZ IFE IFNE IFEZ IFNEZ IFT IFF
%token MATCH FUNCTION BIND IN IMPORT
%token SLASH SQUOTE DQUOTE BQUOTE COMMA SEMICOLON PERIOD
%token LBRACKET RBRACKET LPAREN RPAREN EOF

%token <literal> LITERAL
%token <name> NAME
%token <terminator> TERMINATOR
%token <type_def> TYPE_DEF

%start sentence
%type <Ast.sentence> sentence
%%

list:
  LBRACKET LITERAL

identifier:
  NAME { Name($1) }
| LITERAL { Literal($1) }

pattern_name:
  identifier { PatternName($1) }

pattern:
  identifier pattern { Pattern($1, $2) }
| identifier { Id($1) }
| pattern_name { PatternOp($1) }

match_sform:
  pattern COMMA clause SEMICOLON match_sform { Multiple(SubMatch($1, $3), $5) }
| pattern COMMA clause { Single(SubMatch($1, $3)) }

arg:
  NAME { WithoutTypeDef($1) }
| NAME LPAREN TYPE_DEF RPAREN { WithTypeDef($1, $3) }

args:
  arg args { Args($1, $2) }
| arg { Arg($1) }

expression:
  LITERAL { Literal($1) }
| NAME { Name($1) }
| BIND NAME expression { Bind($2, $3) }
| BIND NAME expression IN expression { BindIn($2, $3, $5) }
| IFGEZ expression COMMA expression { If(GEZ($2, $4)) }
| IFGZ expression COMMA expression { If(GZ($2, $4)) }
| IFLEZ expression COMMA expression { If(LEZ($2, $4)) }
| IFLZ expression COMMA expression { If(LZ($2, $4)) }
| IFE expression COMMA expression { If(Empty($2, $4)) }
| IFNE expression COMMA expression { If(NonEmpty($2, $4)) }
| IFEZ expression COMMA expression { If(EZ($2, $4)) }
| IFNEZ expression COMMA expression { If(NEZ($2, $4)) }
| IFT expression COMMA expression { If(T($2, $4)) }
| IFF expression COMMA expression { If(F($2, $4)) }
| MATCH match_sform { Match($2) }
| LPAREN expression RPAREN { AnonyFunction($2) }
| FUNCTION args COMMA expression { Function($2, $4) }
| IMPORT atom { Import($2) }
| identifier expression { Sequence{$1, $2) }

sentence:
  expression TERMINATOR { Sentence($1, $2) }

