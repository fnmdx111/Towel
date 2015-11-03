
exception LexicalError of (string * int * int);;
exception SyntacticError of (string * int * int * int);;
exception NameNotFoundError of string;;
exception CorruptedScope of string;;

exception TypeError;;

let err s loc stofs eofs = raise (SyntacticError(s,
                                                 loc.Lexing.pos_lnum,
                                                 stofs,
                                                 eofs));;

