{
open Ast
open Parser
open Common
open Lexing
open Exc
}

let _WHITESPACE = [' ' '\t']
let _NEWLINE = '\n' | '\r' | "\r\n"
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
let _AT = "@"
let _LBRACE = '{'
let _RBRACE = '}'

let string_char = [^ '\\' '\'']
let string_esc_charseq = '\\' ['\'' '\\' 'n' 'r' 'b' ' ' 't']
let string_item = string_char | string_esc_charseq
let string_lit = _SQUOTE string_item* _SQUOTE
(* From python lexical analysis
   https://docs.python.org/3/reference/lexical_analysis.html#string-and-bytes-literals*)

let alpha = ['a'-'z']
let reserved_char = [',' ';' '.' '\'' '\\' '`' '@'
                     '(' ')' '[' ']'
                     ' ' '\t' '\n' '\r']
let common_valid_char = ['~' '!' '#' '$' '%' '^' '&' '*' '-' '_' '+' '='
                         '|' ':' '<' '>' '?' '/' 'a'-'z' 'A'-'Z' '0'-'9']
let valid_upper_char = ['~' '!' '#' '$' '%' '^' '&' '*' '_' '+' '='
                        '|' ':' '<' '>' '?' '/' 'A'-'Z']

let name = valid_upper_char common_valid_char*
let atom_lit = alpha common_valid_char*

let digit = ['0'-'9']
let hexdigit = ['0'-'9' 'a'-'f' 'A'-'F']
let bindigit = ['0' '1']
let signed = ['+' '-']
let int_lit = signed? (
              ("0d"? digit+)
            | ("0x" hexdigit+)
            | ("0b" bindigit+))

let dot = '.'
let int = digit+
let frac = digit+
let exp = 'e' signed? int
let dot_float = ((dot frac) | (int dot frac)) exp?
let exp_float = int (dot frac)? exp
let float_lit = signed? (dot_float | exp_float)

rule token = parse
| _WHITESPACE+ { token lexbuf }
| _NEWLINE { Lexing.new_line lexbuf; token lexbuf }
| _BQUOTE { BQUOTE }
| _COMMA { COMMA }
| _SEMICOLON { SEMICOLON }
| _PERIOD { TERMINATOR(Ast.Period) }
| _LPAREN { LPAREN }
| _RPAREN { RPAREN }
| _LBRACKET { LBRACKET }
| _RBRACKET { RBRACKET }
| _SLASH { SLASH }
| _AT { AT }
| _LBRACE { LBRACE }
| _RBRACE { RBRACE }

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
| "bind" { BIND }
| "also" { ALSO }
| "then" { THEN }
| "fun" { FUNCTION }
| "type" { TYPE }

| eof { TERMINATOR(Ast.EOF) }

| _DQUOTE [^ '"' '\n' '\r']* _DQUOTE { token lexbuf } (* comments *)

| name as n {
    NAME({name_ref_key = name_counter ();
          name_repr = n;
          name_type = TypeDef([TDPrimitiveType(PT_Any)])})
  }

              (* literals start here *)
| atom_lit as a {
    ATOM({atom_name = a;
          atom_repr = value_counter ()});
  }
| string_lit as str {
    LITERAL({value_id = value_counter ();
             value_content = VString(String.sub str 1 (String.length str - 2));
             value_type = TypeDef([TDPrimitiveType(PT_String)])})
  }
| int_lit as i {
    LITERAL({value_id = value_counter ();
             value_content = VFixedInt(int_of_string i);
             value_type = TypeDef([TDPrimitiveType(PT_FixedInt)])})
  }
| float_lit as f {
    LITERAL({value_id = value_counter ();
             value_content = VFloat(float_of_string f);
             value_type = TypeDef([TDPrimitiveType(PT_Float)])})
  }

| _ as s {
    raise (LexicalError
             (Printf.sprintf "unexpected character `%c'" s,
              lexbuf.lex_curr_p.pos_lnum,
              lexbuf.lex_curr_p.pos_bol))
  }
