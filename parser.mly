%{ open Ast %}

%token IFGEZ IFGZ IFLEZ IFLZ IFE IFNE IFEZ IFNEZ IFT IFF
%token MATCH FUNCTION BIND IN IMPORT FTO AT
%token SLASH BQUOTE COMMA SEMICOLON EOF
%token LBRACKET RBRACKET LPAREN RPAREN

%token <Ast.pvalue> LITERAL
%token <Ast.name> NAME
%token <Ast.terminator> TERMINATOR

%start sentence
%type <Ast.sentence> sentence
%%

lit_list:
  LBRACKET list(word) RBRACKET { VList($2) }

literal:
  LITERAL { $1 }
| lit_list { {value_id = 1;
              value_content = $1;
              value_type = TypeDef([TDPrimitiveType(PT_List)])} }

sequence:
  LPAREN list(word) RPAREN { Sequence($2) }

backquote:
  BQUOTE literal { BQValue($2) }
| BQUOTE NAME { BQName($2) }
| BQUOTE sequence { BQSeq($2) }

type_def:
  separated_nonempty_list(FTO, NAME) {
    TypeDef(List.map (fun x -> TDName(x)) $1)
  }

arg_def:
  NAME { ArgDef($1) }
| NAME AT type_def { ArgDefWithType($1, $3) }

if_sform:
  IFGEZ list(word) COMMA list(word) { IfGEZ(IfBody($2, $4)) }
| IFGZ list(word) COMMA list(word) { IfGZ(IfBody($2, $4)) }
| IFLEZ list(word) COMMA list(word) { IfLEZ(IfBody($2, $4)) }
| IFLZ list(word) COMMA list(word) { IfLZ(IfBody($2, $4)) }
| IFE list(word) COMMA list(word) { IfEmpty(IfBody($2, $4)) }
| IFNE list(word) COMMA list(word) { IfNonEmpty(IfBody($2, $4)) }
| IFEZ list(word) COMMA list(word) { IfEZ(IfBody($2, $4)) }
| IFNEZ list(word) COMMA list(word) { IfNEZ(IfBody($2, $4)) }
| IFT list(word) COMMA list(word) { IfT(IfBody($2, $4)) }
| IFF list(word) COMMA list(word) { IfF(IfBody($2, $4)) }

pattern:
  list(word) COMMA list(word) { PatternAndMatch($1, $3) }

match_sform:
  MATCH separated_nonempty_list(SEMICOLON, pattern) {
    PatternsAndMatches($2)
  }

control_sequence:
  if_sform { CtrlSeqIfForm($1) }
| match_sform { CtrlSeqMatchForm($1) }

other_form:
  FUNCTION list(arg_def) COMMA list(word) { Function($2, $4) }
| BIND NAME list(word) { Bind($2, $3) }
| BIND NAME list(word) IN list(word) { BindIn($2, $3, $5) }
| word IMPORT { Import($1) }
| word AT word { At($1, $3) }

word:
  NAME { WName($1) }
| literal { WLiteral($1) }
| backquote { WBackquote($1) }
| sequence { WSequence($1) }
| control_sequence { WControl($1) }
| other_form { WOtherForm($1) }

sentence:
  list(word) TERMINATOR { Sentence($1, $2) }
