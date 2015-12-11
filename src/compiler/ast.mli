open Stdint

type atom = {
  atom_name: string;
  atom_repr: int
}

and pname = {
  name_repr: string;
}

and name =
    NRegular of pname list
  | NTailCall of pname list

type pvalue = {value_content: pvalue_content}
and pvalue_content =
    VAtom of atom
  | VFixedInt of int64
  | VInt of Big_int.big_int
  | VUFixedInt of uint64
  | VFloat of float
  | VList of word list
  | VString of string
  | VTuple of word list

and backquote =
    BQValue of pvalue
  | BQName of name
  | BQSeq of sequence
  | BQBackquote of backquote

and import =
    ExplicitImport of string list
  | ImplicitImport of string list

and word =
    WLiteral of pvalue
  | WName of name
  | WBackquote of backquote
  | WSequence of sequence
  | WControl of control_sequence
  | WFunction of function_sform
  | WImport of import
  | WExport of pname list
  | WBind of bind_sform

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

and control_sequence =
    CtrlSeqIfForm of if_sform

and arg_def =
    ArgDef of pname

and function_sform =
    Function of arg_def list * word
  | BQFunction of arg_def list * word

and bind_body = BindBody of pname * word

and bind_sform = BindThen of bind_body list * word

and terminator = EOF

type sentence = Sentence of word list;;
