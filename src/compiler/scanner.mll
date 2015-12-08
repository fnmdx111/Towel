{
open Ast
open Parser
open Common
open Lexing
open Exc
open Stdint
open Batteries

let strip_mod s =
  let len = String.length s
  in String.sub s 0 (len - 1)

let strip_sign s =
  let len = String.length s
  in let first = String.sub s 0 1
  in if (first = "+") || (first = "-")
  then String.sub s 1 @@ len - 1
  else s

let unquote s =
  let rec _unq output state = function
      ch::rest ->
      if ch <> '\\'
      then if state = 0
        then (BatIO.write output ch;
              _unq output 0 rest)
        else (BatIO.write output
                (match ch with
                   'n' -> '\n'
                 | 't' -> '\t'
                 | 'b' -> '\b'
                 | '\'' -> '\''
                 | '\\' -> '\\'
                 | _ -> failwith "Illegal escape sequence.");
              _unq output 0 rest)
      else _unq output 1 rest
    | [] -> BatIO.close_out output
  in let out = BatIO.output_string ()
  in _unq out 0 (String.to_list s)
}

let _WHITESPACE = [' ' '\t']
let _NEWLINE = '\n' | '\r' | "\r\n"
let _SQUOTE = '\''
let _DQUOTE = '"'
let _BQUOTE = '`'
let _COMMA = ','
let _SEMICOLON = ';'
let _LPAREN = '('
let _RPAREN = ')'
let _LBRACKET = '['
let _RBRACKET = ']'
let _SLASH = '\\'
let _AT = '@'
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
                     '(' ')' '[' ']' '{' '}'
                     ' ' '\t' '\n' '\r']
let common_valid_char = ['~' '!' '#' '$' '%' '^' '&' '*' '-' '_' '+' '=' '.'
                         '|' ':' '<' '>' '?' '/' 'a'-'z' 'A'-'Z' '0'-'9']
let common_valid_char_no_digits =
  ['~' '!' '#' '$' '%' '^' '&' '*' '-' '_' '+' '=' '.'
   '|' ':' '<' '>' '?' '/' 'a'-'z' 'A'-'Z']

let valid_upper_char = ['~' '!' '#' '$' '%' '^' '&' '*' '_' '=' '.'
                        '|' ':' '<' '>' '?' '/' 'A'-'Z']

let digit = ['0'-'9']
let hexdigit = ['0'-'9' 'a'-'f' 'A'-'F']
let bindigit = ['0' '1']
let signed = ['+' '-']
let _int_lit = (("0d"? digit+)
              | ("0x" hexdigit+)
              | ("0b" bindigit+))
let fint_lit = signed? _int_lit
let int_lit = signed? digit+ ['L' 'l']
let ufint_lit = '+'? _int_lit ['U' 'u']

let dot = '.'
let int = digit+
let frac = digit+
let exp = 'e' signed? int
let dot_float = ((dot frac) | (int dot frac)) exp?
let exp_float = int (dot frac)? exp
let float_lit = signed? (dot_float | exp_float)

let name = (valid_upper_char common_valid_char*)
         | ('+' common_valid_char_no_digits?)
         | ('+' common_valid_char_no_digits common_valid_char*)
         | ('-' common_valid_char_no_digits?)
         | ('-' common_valid_char_no_digits common_valid_char*)
let atom_lit = alpha common_valid_char*


rule token = parse
| _WHITESPACE+ { token lexbuf }
| _NEWLINE { Lexing.new_line lexbuf; token lexbuf }
| _BQUOTE { BQUOTE }
| _COMMA { COMMA }
| _SEMICOLON { SEMICOLON }
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
| "import" { IMPORT }
| "export" { EXPORT }
| ",\\" { LAMBDA }

| eof { TERMINATOR(Ast.EOF) }

| _DQUOTE { comment lexbuf } (* comments *)

| name as n {
    NAME({name_repr = n})
  }

              (* literals start here *)
| atom_lit as a {
    ATOM({atom_name = a;
          atom_repr = 1});
  }
| string_lit as str {
    STRING(unquote(String.sub str 1 (String.length str - 2)))
  }
| fint_lit as i {
    LITERAL({value_content = VFixedInt(Int64.of_string i)})
  }
| int_lit as i {
    LITERAL({value_content = VInt(Big_int.big_int_of_string @@ strip_mod i)})
  }
| ufint_lit as i {
    LITERAL({value_content = VUFixedInt(Uint64.of_string
                                        (i |> strip_sign |> strip_mod))})
  }
| float_lit as f {
    LITERAL({value_content = VFloat(float_of_string f)})
  }

| _ as s {
    raise (LexicalError
             (Printf.sprintf "unexpected character `%c'" s,
              lexbuf.lex_curr_p.pos_lnum,
              lexbuf.lex_curr_p.pos_bol))
  }

and comment = parse
  _DQUOTE { token lexbuf }
| _ { comment lexbuf }
