
type primitive_type =
    PT_Atom
  (*  | Int *)
  | PT_FixedInt
  | PT_Float
  | PT_String
  | PT_List
  | PT_Any;;


type atom = {
  atom_name: string;
  atom_repr: int
};;

type type_def =
    TypeDef of type_def_item list

and type_def_item = TDName of name | TDPrimitiveType of primitive_type

and name = { mutable
  name_ref_key: int;
  name_repr: string;
  name_type: type_def
};;

type pvalue = {value_id: int;
               value_content: pvalue_content;
               value_type: type_def}
and pvalue_content =
    VAtom of atom
  | VFixedInt of int
  | VFloat of float
  | VList of word list
  | VString of string

and backquote =
  BQValue of pvalue
| BQName of name
| BQSeq of sequence
| BQBackquote of backquote

and word =
  WLiteral of pvalue
| WName of name
| WBackquote of backquote
| WSequence of sequence
| WControl of control_sequence
| WOtherForm of other_form

and sequence =
  Sequence of word list

and if_body = IfBody of word list * word list

and if_sform =
    IfGEZ of if_body
  | IfGZ of if_body
  | IfLEZ of if_body
  | IfLZ of if_body
  | IfEmpty of if_body
  | IfNonEmpty of if_body
  | IfEZ of if_body
  | IfNEZ of if_body
  | IfT of if_body
  | IfF of if_body

and pattern = PatternAndMatch of word list * word list
and match_sform = PatternsAndMatches of pattern list

and control_sequence =
  CtrlSeqIfForm of if_sform
| CtrlSeqMatchForm of match_sform

and arg_def =
  ArgDef of name
| ArgDefWithType of name * type_def

and other_form =
  Bind of name * word list
| BindIn of name * word list * word list
| Function of arg_def list * word list
| Import of word
| At of word * word

and terminator = Period

type sentence = Sentence of word list * terminator;;
