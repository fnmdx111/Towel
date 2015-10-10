%{ open Ast
   open Common %}

%token IFGEZ IFGZ IFLEZ IFLZ IFE IFNE IFEZ IFNEZ IFT IFF
%token MATCH FUNCTION BIND IN FTO AT
%token SLASH BQUOTE COMMA SEMICOLON
%token LBRACKET RBRACKET LPAREN RPAREN

%token <Ast.pvalue> LITERAL
%token <Ast.name> NAME
%token <Ast.terminator> TERMINATOR

%right BIND
%left IN


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
  literal BQUOTE { BQValue($1) }
| name BQUOTE { BQName($1) }
| sequence BQUOTE { BQSeq($1) }
| backquote BQUOTE { BQBackquote($1) }

type_def:
  separated_nonempty_list(FTO, name) {
    TypeDef(List.map (fun x -> TDName(x)) $1)
  }

arg_def:
  name { ArgDef($1) }
| name AT type_def { ArgDefWithType($1, $3) }

if_sform:
  IFGEZ word COMMA word { IfGEZ(IfBody($2, $4)) }
| IFGZ word COMMA word { IfGZ(IfBody($2, $4)) }
| IFLEZ word COMMA word { IfLEZ(IfBody($2, $4)) }
| IFLZ word COMMA word { IfLZ(IfBody($2, $4)) }
| IFE word COMMA word { IfEmpty(IfBody($2, $4)) }
| IFNE word COMMA word { IfNonEmpty(IfBody($2, $4)) }
| IFEZ word COMMA word { IfEZ(IfBody($2, $4)) }
| IFNEZ word COMMA word { IfNEZ(IfBody($2, $4)) }
| IFT word COMMA word { IfT(IfBody($2, $4)) }
| IFF word COMMA word { IfF(IfBody($2, $4)) }

pattern:
  list(word) COMMA restricted_word { PatternAndMatch($1, $3) }

match_sform:
  MATCH separated_nonempty_list(SEMICOLON, pattern) {
    PatternsAndMatches($2)
  }

control_sequence:
  if_sform { CtrlSeqIfForm($1) }
| match_sform { CtrlSeqMatchForm($1) }

name:
  NAME SLASH NAME {
    promote_name_to_module_name $3;
    $1.name_domain <- SomeModule({module_name = $3;
                                  module_path = ""});
    $1 }
| NAME { $1 }

restricted_word:
  name { WName($1) }
| backquote { WBackquote($1) }
| LITERAL { WLiteral($1) }
| sequence { WSequence($1) }

function_:
  FUNCTION list(arg_def) COMMA word { Function($2, $4) }

other_form:
  BIND name word IN word { BindIn($2, $3, $5) }
| BIND name word { Bind($2, $3) }
| function_ { $1 }
| restricted_word AT word { At($1, $3) }

word:
  backquote { WBackquote($1) }
| sequence { WSequence($1) }
| literal { WLiteral($1) }
| control_sequence { WControl($1) }
| other_form { WOtherForm($1) }
| name { WName($1) }

sequence:
  LPAREN list(word) RPAREN { Sequence($2) }

sentence:
  list(word) TERMINATOR { Sentence($1, $2) }
