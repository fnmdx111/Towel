%{
open Ast
open Common

let err s loc stofs eofs = raise (SyntacticError(s,
                                                 loc.Lexing.pos_lnum,
                                                 stofs,
                                                 eofs));;
%}

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
| LBRACKET list(word) error {
    err "missing right bracket for list literals"
      $startpos($3) $startofs($1) $endofs($3)
  }

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
| error {
    err "unexpected type definition" $startpos($1) $startofs($1) $endofs($1)
  }

arg_def:
  NAME { ArgDef($1) }
| NAME AT type_def { ArgDefWithType($1, $3) }

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
| IFGEZ word error {
    err "expecting comma for else branch"
    $startpos($3) $startofs($1) $endofs($3)
  }
| IFGZ word error {
    err "expecting comma for else branch"
    $startpos($3) $startofs($1) $endofs($3)
  }
| IFLEZ word error {
    err "expecting comma for else branch"    
    $startpos($3) $startofs($1) $endofs($3)
  }
| IFLZ word error {
    err "expecting comma for else branch"
    $startpos($3) $startofs($1) $endofs($3)
  }
| IFE word error {
    err "expecting comma for else branch"
    $startpos($3) $startofs($1) $endofs($3)
  }
| IFNE word error {
    err "expecting comma for else branch"
    $startpos($3) $startofs($1) $endofs($3)
  }
| IFEZ word error {
    err "expecting comma for else branch"
    $startpos($3) $startofs($1) $endofs($3)
  }
| IFNEZ word error {
    err "expecting comma for else branch"
    $startpos($3) $startofs($1) $endofs($3)
  }
| IFT word error {
    err "expecting comma for else branch"
    $startpos($3) $startofs($1) $endofs($3)
  }
| IFF word error {
    err "expecting comma for else branch"
    $startpos($3) $startofs($1) $endofs($3)
  }
| IFGEZ word COMMA error {
    err "unexpected form for else branch"
    $startpos($3) $startofs($1) $endofs($3)
  }
| IFGZ word COMMA error {
    err "unexpected form for else branch"
    $startpos($3) $startofs($1) $endofs($3)
  }
| IFLEZ word COMMA error {
    err "unexpected form for else branch"
    $startpos($3) $startofs($1) $endofs($3)
  }
| IFLZ word COMMA error {
    err "unexpected form for else branch"
    $startpos($3) $startofs($1) $endofs($3)
  }
| IFEZ word COMMA error {
    err "unexpected form for else branch"
    $startpos($3) $startofs($1) $endofs($3)
  }
| IFNEZ word COMMA error {
    err "unexpected form for else branch"
    $startpos($3) $startofs($1) $endofs($3)
  }
| IFE word COMMA error {
    err "unexpected form for else branch"
    $startpos($3) $startofs($1) $endofs($3)
  }
| IFNE word COMMA error {
    err "unexpected form for else branch"
    $startpos($3) $startofs($1) $endofs($3)
  }
| IFT word COMMA error {
    err "unexpected form for else branch"
    $startpos($3) $startofs($1) $endofs($3)
  }
| IFF word COMMA error {
    err "unexpected form for else branch"
    $startpos($3) $startofs($1) $endofs($3)
  }

pattern:
  list(word) COMMA restricted_word { PatternAndMatch($1, $3) }

match_sform:
  MATCH separated_nonempty_list(SEMICOLON, pattern) {
    PatternsAndMatches($2)
  }
| MATCH error {
    err "expected pairs of pattern-and-actions"
    $startpos($2) $startofs($1) $endofs($2)
  }

control_sequence:
  if_sform { CtrlSeqIfForm($1) }
| match_sform { CtrlSeqMatchForm($1) }

name:
  NAME SLASH name {
    promote_name_to_module_name $3;
    $1.name_domain <- SomeModule({module_name = $3;
                                  module_path = ""});
    $1 }
| NAME { $1 }
| NAME SLASH error {
    err "expected a name (possibly with a namespace reference)"
    $startpos($3) $startofs($1) $endofs($3)
  }

restricted_word:
  name { WName($1) }
| backquote { WBackquote($1) }
| literal { WLiteral($1) }
| sequence { WSequence($1) }

function_:
  FUNCTION list(arg_def) COMMA word { Function($2, $4) }
| FUNCTION list(arg_def) error {
    err "missing comma for function special form"
    $startpos($3) $startofs($1) $endofs($3)
  }

bind_sform:
  BIND name word IN word { BindIn($2, $3, $5) }
| BIND name word { Bind($2, $3) }
| BIND name word IN error {
    err "missing form for bind-in special form"
    $startpos($5) $startofs($1) $endofs($5)
  }
| BIND error {
    err "expected a name" $startpos($2) $startofs($1) $endofs($2)
  }

at_sform:
  restricted_word AT word { At($1, $3) }
| restricted_word AT error {
    err "unexpected form" $startpos($1) $startofs($1) $endofs($1)
  }

word:
  backquote { WBackquote($1) }
| sequence { WSequence($1) }
| literal { WLiteral($1) }
| control_sequence { WControl($1) }
| function_ { WFunction($1) }
| bind_sform { WBind($1) }
| at_sform { WAt($1) }
| name { WName($1) }

sequence:
  LPAREN list(word) RPAREN { Sequence($2) }
| LPAREN list(word) error {
    err "missing right parenthesis for sequences"
    $startpos($3) $startofs($1) $endofs($3)
  }

sentence:
  list(word) TERMINATOR { Sentence($1, $2) }
| list(word) error {
    err "expected a terminator"
    $startpos($2) $startofs($1) $endofs($2)
  }
