
type primitive_type =
    PT_Atom
  (*  | Int *)
  | PT_FixedInt
  | PT_Float
  | PT_String
  | PT_List
  | PT_Any;;


type type_def =
    Def of primitive_type * type_def
  | SingleDef of primitive_type;;

type atom = {
  atom_name: string;
  atom_repr: int
};;

type name = { mutable
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
and word =
  WLiteral of pvalue
| WName of name
| WBackquote of backquote
| WSequence of sequence
and sequence =
  Sequence of word list

type sentence =
  Sentence of word list
| EOF

type if_body = IfBody of word * word;;

type if_sform =
    GEZ of if_body
  | GZ of if_body
  | LEZ of if_body
  | LZ of if_body
  | Empty of if_body
  | NonEmpty of if_body
  | EZ of if_body
  | NEZ of if_body
  | T of if_body
  | F of if_body;;

(* type pattern_name = PatternName of identifier;; *)

(* type pattern = *)
(*     Pattern of identifier * pattern *)
(*   | Id of identifier *)
(*   | PatternOp of pattern_name;; *)

(* type match_clause = SubMatch of pattern * expression;; *)

(* type match_sform = *)
(*     Multiple of match_clause * match_sform *)
(*   | Single of match_clause;; *)

(* type arg = *)
(*     WithoutTypeDef of name *)
(*   | WithTypeDef of name * type_def;; *)

(* type args = *)
(*     Args of arg * args *)
(*   | Arg of arg;; *)

(* type expression = *)
(*     Literal of literal *)
(*   | EmptyExpression *)
(*   | Name of name *)
(*   | Bind of name * expression *)
(*   | BindIn of name * expression * expression *)
(*   | If of if_sform *)
(*   | Match of match_sform *)
(*   | AnonyFunction of expression *)
(*   | Function of args * expression *)
(*   | Import of name *)
(*   | Sequence of identifier * expression;; *)

type terminator =
  Period | Exclamation | Question;;

(* type sentence = Sentence of expression * terminator;; *)
      
