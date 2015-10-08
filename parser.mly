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

pattern:
  word COMMA word { PatternAndMatch($1, $3) }

if_sform:
  IFGEZ word word { IfGEZ(IfBody($2, $3)) }
| IFGZ word word { IfGZ(IfBody($2, $3)) }
| IFLEZ word word { IfLEZ(IfBody($2, $3)) }
| IFLZ word word { IfLZ(IfBody($2, $3)) }
| IFE word word { IfEmpty(IfBody($2, $3)) }
| IFNE word word { IfNonEmpty(IfBody($2, $3)) }
| IFEZ word word { IfEZ(IfBody($2, $3)) }
| IFNEZ word word { IfNEZ(IfBody($2, $3)) }
| IFT word word { IfT(IfBody($2, $3)) }
| IFF word word { IfF(IfBody($2, $3)) }

match_sform:
  MATCH separated_nonempty_list(SEMICOLON, pattern) {
    PatternsAndMatches($2)
  }

control_sequence:
  if_sform { CtrlSeqIfForm($1) }
| match_sform { CtrlSeqMatchForm($1) }

other_form:
  BIND NAME word { Bind($2, $3) }
| BIND NAME word IN word { BindIn($2, $3, $5) }
| FUNCTION list(arg_def) word { Function($2, $3) }
| word IMPORT { Import($1) }
| word AT word { At($1, $3) }

word:
  backquote { WBackquote($1) }
| sequence { WSequence($1) }
| literal { WLiteral($1) }
| control_sequence { WControl($1) }
| other_form { WOtherForm($1) }
| NAME { WName($1) }

sequence:
  LPAREN list(word) RPAREN { Sequence($2) }

sentence:
  list(word) TERMINATOR { Sentence($1, $2) }
