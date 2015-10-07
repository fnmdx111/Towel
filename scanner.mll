{
open Parser

let atom_counter =
    let cnt = Array.of_list [-1]
    in fun () -> cnt.(0) <- cnt.[0] + 1; cnt.(0);;

let name_counter =
    let cnt = Array.of_list [-1]
    in fun () -> cnt.(0) <- cnt.[0] + 1; cnt.(0);;
}

let _WHITESPACE = [' ' '\t']
let _NELINE = '\n' | '\r' | "\r\n"
let _SQUOTE = '\''
let _DQUOTE = '"'
let _BQUOTE = '`'
let _COMMA = ','
let _SEMICOLON = ';'
let _PERIOD = '.'
let _LPAREN = '('
let _RPAREN = ')'
let _LBRACKET = '['
let _RBRACKET = ']'
let _SLASH = '\\'

let string_char = [^ '\\' '\'']
let string_esc_charseq = '\\' string_char
let string_item = string_char | string_esc_charseq
let string_lit = _SQUOTE string_item* _SQUOTE
(* from python lexical analysis
   https://docs.python.org/3/reference/lexical_analysis.html#string-and-bytes-literals*)

let alpha = ['a'-'z']
let common_legal_char = [^ ',' ';' '.' '\'' '\\' '`'
                          '(' ')' '[' ']'
                          ' ' '\t' '\n' '\r']
let name = [^ ',' ';' '.' '\\' '\'' '`'
              '(' ')' '[' ']'
              'a'-'z' '0'-'9'
              ' ' '\t' '\n' '\r'] common_legal_char*
let atom_lit = alpha common_legal_char*

let digit = ['0'-'9']
let signed = ['+' '-']
let int_lit = signed? digit+
let frac = '.' digit*
let float_lit = signed? digit+ frac? ('e' digit+)?

rule token = parse
| _SQUOTE { SQUOTE }
| _DQUOTE { DQUOTE }
| _BQUOTE { BQUOTE }
| _COMMA { COMMA }
| _SEMICOLON { SEMICOLON }
| _PERIOD { PERIOD }
| _LPAREN { LPAREN }
| _RPAREN { RPAREN }
| _LBRACKET { LBRACKET }
| _RBRACKET { RBRACKET }
| _SLASH { SLASH }

| "if>=0" { IFGEZ }
| "if>0" { IFGZ }
| "if<=0" { IFLEZ }
| "if<0" { IFLZ }
| "if=0" { IFEZ }
| "if~0" { IFNEZ }
| "ift" { IFT }
| "iff" { IFF }
| "ife" { IFE }
| "ifne" { IFNE }
| "match" { MATCH }
| "function" { FUNCTION }
| "bind" { BIND }
| "in" { IN }
| "import" { IMPORT }

| eof { EOF }

| _DQUOTE [^ '"'] _DQUOTE { token lexbuf }

              (* literals start here *)
| string_lit as str {
    LITERAL(LString(String.sub str 1 (String.length str - 2)))
  }
| name as n {
	NAME({name_repr: n; name_ref_key: name_counter (); name_type: SingleDef(Any)})
  }
| int_lit as i { LITERAL(LFixedInt(int_of_string i)) }
| float_lit as f { LITERAL(LFloat(float_of_string f)) }
| atom_lit as a { LITERAL(LAtom({atom_name: a; atom_repr: atom_counter ()) }
