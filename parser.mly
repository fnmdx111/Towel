%{
open Ast
open Common
open Exc

%}

%token IFGEZ IFGZ IFLEZ IFLZ IFE IFNE IFEZ IFNEZ IFT IFF
%token MATCH FUNCTION BIND ALSO THEN AT TYPE
%token SLASH BQUOTE COMMA SEMICOLON
%token LBRACKET RBRACKET LPAREN RPAREN LBRACE RBRACE

%token <Ast.atom> ATOM
%token <Ast.pvalue> LITERAL
%token <Ast.name> NAME
%token <Ast.terminator> TERMINATOR

%start sentence
%type <Ast.sentence> sentence
%%

lit_list:
  LBRACKET list(word) RBRACKET { VList($2) }
| LBRACKET list(word) error {
    err "expected a right bracket for list literal"
      $startpos($3) $startofs($1) $endofs($3)
  }

lit_tuple:
  LBRACKET SLASH list(word) RBRACKET { VTuple($3) }

lit_altype_literal_constructor:
  ATOM SLASH name { AlTypeLiteralConstructor($1, $3) }

lit_altype_literal_item:
  word { AlTypeLiteralItemWord($1) }
| lit_altype_literal_constructor { AlTypeLiteralItemConstructor($1) }

lit_altype_literal:
  LBRACKET AT nonempty_list(lit_altype_literal_item) RBRACKET
    lit_altype_literal_constructor { AlTypeLiteral($3, $5) }

literal:
  LITERAL { $1 }
| ATOM { {value_id = 1; value_content = VAtom($1);
          value_type = TypeDef([TDPrimitiveType(PT_Atom)])} }
| lit_list { {value_id = 1;
              value_content = $1;
              value_type = TypeDef([TDPrimitiveType(PT_List)])} }
| lit_tuple { {value_id = 1;
               value_content = $1;
               value_type = TypeDef([TDPrimitiveType(
                   PT_Tuple(List.length (match $1 with
                         VTuple(ws) -> ws
                       | _ -> [])))])} }
| lit_altype_literal { {value_id = 1;
                        value_content = VAlTypeLiteral($1);
                        value_type = TypeDef([TDPrimitiveType(PT_Any)])} }

backquote:
  literal BQUOTE { BQValue($1) }
| name BQUOTE { BQName($1) }
| sequence BQUOTE { BQSeq($1) }
| backquote BQUOTE { BQBackquote($1) }
| LBRACE list(word) RBRACE { BQSeq(SharedSequence($2)) }

type_def:
  LPAREN nonempty_list(name) RPAREN {
    TypeDef(List.map (fun x -> TDName(x)) $2)
  }
| LPAREN error {
    err "expected names for type definition"
      $startpos($2) $startofs($1) $endofs($2)
  }
| LPAREN nonempty_list(name) error {
    err "expected a right parenthesis for type definition"
    $startpos($3) $startofs($1) $endofs($3)
  }

arg_def:
  NAME { ArgDef($1) }
| NAME type_def { ArgDefWithType($1, $2) }

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
    err "expected a comma for else branch"
    $startpos($3) $startofs($1) $endofs($3)
  }
| IFGZ word error {
    err "expected a comma for else branch"
    $startpos($3) $startofs($1) $endofs($3)
  }
| IFLEZ word error {
    err "expected a comma for else branch"    
    $startpos($3) $startofs($1) $endofs($3)
  }
| IFLZ word error {
    err "expected a comma for else branch"
    $startpos($3) $startofs($1) $endofs($3)
  }
| IFE word error {
    err "expected a comma for else branch"
    $startpos($3) $startofs($1) $endofs($3)
  }
| IFNE word error {
    err "expected a comma for else branch"
    $startpos($3) $startofs($1) $endofs($3)
  }
| IFEZ word error {
    err "expected a comma for else branch"
    $startpos($3) $startofs($1) $endofs($3)
  }
| IFNEZ word error {
    err "expected a comma for else branch"
    $startpos($3) $startofs($1) $endofs($3)
  }
| IFT word error {
    err "expected a comma for else branch"
    $startpos($3) $startofs($1) $endofs($3)
  }
| IFF word error {
    err "expected a comma for else branch"
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
| IFGEZ error {
    err "unexpected form for if branch"
    $startpos($2) $startofs($1) $endofs($2)
  }
| IFGZ error {
    err "unexpected form for if branch"
    $startpos($2) $startofs($1) $endofs($2)
  }
| IFLEZ error {
    err "unexpected form for if branch"
    $startpos($2) $startofs($1) $endofs($2)
  }
| IFLZ error {
    err "unexpected form for if branch"
    $startpos($2) $startofs($1) $endofs($2)
  }
| IFE error {
    err "unexpected form for if branch"
    $startpos($2) $startofs($1) $endofs($2)
  }
| IFNE error {
    err "unexpected form for if branch"
    $startpos($2) $startofs($1) $endofs($2)
  }
| IFT error {
    err "unexpected form for if branch"
    $startpos($2) $startofs($1) $endofs($2)
  }
| IFF error {
    err "unexpected form for if branch"
    $startpos($2) $startofs($1) $endofs($2)
  }
| IFEZ error {
    err "unexpected form for if branch"
    $startpos($2) $startofs($1) $endofs($2)
  }
| IFNEZ error {
    err "unexpected form for if branch"
    $startpos($2) $startofs($1) $endofs($2)
  }

pattern:
  list(restricted_word) COMMA restricted_word { PatternAndMatch($1, $3) }
| list(restricted_word) COMMA error {
    err "unexpected action clause form"
      $startpos($3) $startofs($1) $endofs($3)
  }
(*
 why does this rule have something to do with conflicts of a function rule?
| list(word) error {
    err "expected a comma for action clause"
      $startpos($2) $startofs($1) $endofs($2)
  }
*)

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
    err "expected a comma for function special form"
    $startpos($3) $startofs($1) $endofs($3)
  }

bind_body:
  NAME word { BindBody($1, $2) }

bind_sform:
  BIND separated_nonempty_list(ALSO, bind_body) THEN word {
    BindThen($2, $4)
  }
| BIND separated_nonempty_list(ALSO, bind_body) error {
    err "expected \"then\" for bind-then special form"
    $startpos($3) $startofs($1) $endofs($3)
  }
| BIND separated_nonempty_list(ALSO, bind_body) THEN error {
    err "expected a form for bind-then special form"
    $startpos($4) $startofs($1) $endofs($4)
  }
| BIND error {
    err "expected a name" $startpos($2) $startofs($1) $endofs($2)
  }

at_sform:
  restricted_word AT word { At($1, $3) }
| restricted_word AT error {
    err "unexpected form" $startpos($1) $startofs($1) $endofs($1)
  }

altype_parameter:
  LBRACE nonempty_list(ATOM) RBRACE { AlTypeParameter($2) }

altype_case_def_item:
  ATOM { AlTypeCaseDefItemAtom($1) }
| NAME { AlTypeCaseDefItemName($1) }
| altype_parameter NAME { AlTypeCaseDefItemNameWithParameter($2, $1) }

altype_case_def:
  LBRACKET AT list(altype_case_def_item) RBRACKET ATOM {
    AlTypeCaseDef($3, $5)
  }
| ATOM { AlTypeCaseDef([], $1) }

altype_def:
  NAME separated_nonempty_list(COMMA, altype_case_def) {
    AlTypeDef($1, $2)
  }

altype_sform:
  TYPE separated_nonempty_list(ALSO, altype_def) THEN word {
    AlType($2, $4)
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
| altype_sform { WAlType($1) }

sequence:
  LPAREN list(word) RPAREN { Sequence($2) }
| LPAREN AT list(word) RPAREN { SharedSequence($3) }
| LPAREN AT error {
    err "expected a list of words for shared sequence"
    $startpos($3) $startofs($1) $endofs($3)
  }
| LPAREN list(word) error {
    err "expected right parenthesis for sequence"
    $startpos($3) $startofs($1) $endofs($3)
  }

sentence:
  list(word) TERMINATOR { Sentence($1, $2) }
| list(word) error {
    err "expected a terminator"
    $startpos($2) $startofs($2) $endofs($2)
  }
