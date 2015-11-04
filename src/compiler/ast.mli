open Stdint

type primitive_type =
    PT_Atom
  | PT_FixedInt
  | PT_Int
  | PT_UFixedInt
  | PT_Float
  | PT_String
  | PT_List
  | PT_Module
  | PT_Number
  | PT_Tuple of int
  | PT_Any;;

type atom = {
  atom_name: string;
  atom_repr: int
};;

type type_def =
    TypeDef of type_def_item list

and type_def_item = TDName of name | TDPrimitiveType of primitive_type

and pname = {
  name_repr: string;
  mutable name_type: type_def
}

and name =
    NRegular of pname list
  | NTailCall of pname list

type pvalue = {value_content: pvalue_content;
               value_type: type_def}
and pvalue_content =
    VAtom of atom
  | VFixedInt of int64
  | VInt of Big_int.big_int
  | VUFixedInt of uint64
  | VFloat of float
  | VList of word list
  | VString of string
  | VTuple of word list
  | VAlTypeLiteral of altype_lit

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
  | WFunction of function_sform
  | WAt of at_sform
  | WBind of bind_sform
  | WAlType of altype_sform
  | WIdle

and sequence =
    SharedSequence of word list
  | Sequence of word list

and if_body = IfBody of word * word
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

and pattern = PatternAndMatch of word list * word
and match_sform = PatternsAndMatches of pattern list

and control_sequence =
    CtrlSeqIfForm of if_sform
  | CtrlSeqMatchForm of match_sform

and arg_def =
    ArgDef of pname
  | ArgDefWithType of pname * type_def

and function_sform =
    Function of arg_def list * word

and bind_body = BindBody of pname * word

and bind_sform = BindThen of bind_body list * word

and altype_parameter = AlTypeParameter of atom list
and altype_case_def_item =
    AlTypeCaseDefItemAtom of atom
  | AlTypeCaseDefItemName of pname
  | AlTypeCaseDefItemNameWithParameter of pname * altype_parameter
and altype_case_def = AlTypeCaseDef of altype_case_def_item list * atom
and altype_def = AlTypeDef of pname * altype_case_def list
and altype_sform = AlType of altype_def list * word

and altype_lit_constructor = AlTypeLiteralConstructor of atom * name
and altype_lit_item = AlTypeLiteralItemWord of word
                    | AlTypeLiteralItemConstructor of altype_lit_constructor
and altype_lit =
    AlTypeLiteral of altype_lit_item list * altype_lit_constructor

and at_sform =
    At of word * word

and terminator = Period | EOF

and export = Export of pname list;;

type sentence = Sentence of word list
              | Export of pname list;;
