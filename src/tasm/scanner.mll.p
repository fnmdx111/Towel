{
open Tasm_ast
open Tasm_parser
open Stdint

let strip_mod s =
  let len = String.length s
  in String.sub s 0 (len - 1)

let strip_sign s =
  let len = String.length s
  in let first = String.sub s 0 1
  in if (first = "+") || (first = "-")
  then String.sub s 1 @@ len - 1
  else s
}

let _WHITESPACE = [' ' '\t']
let _NEWLINE = '\n' | '\r' | "\r\n"
let _SQUOTE = '\''
let _DQUOTE = '"'

let string_char = [^ '\\' '\'']
let string_esc_charseq = '\\' ['\'' '\\' 'n' 'r' 'b' ' ' 't']
let string_item = string_char | string_esc_charseq
let string_lit = _SQUOTE string_item* _SQUOTE
(* From python lexical analysis
   https://docs.python.org/3/reference/lexical_analysis.html#string-and-bytes-literals*)

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
let atom_lit = _int_lit ['A' 'a']

let dot = '.'
let int = digit+
let frac = digit+
let exp = 'e' signed? int
let dot_float = ((dot frac) | (int dot frac)) exp?
let exp_float = int (dot frac)? exp
let float_lit = signed? (dot_float | exp_float)

let label_lit = ":" ['0'-'9' 'a'-'z' 'A'-'Z' '-']+ "!"?

rule token = parse
| _WHITESPACE+ { token lexbuf }
| _NEWLINE { Lexing.new_line lexbuf; token lexbuf }

| eof { EOF }

| _DQUOTE [^ '"' '\n' '\r']* _DQUOTE { token lexbuf } (* comments *)

| label_lit as a { LABEL(Tasm_ast.Label(a)) }
| string_lit as str {
    LITERAL(VString(String.sub str 1 (String.length str - 2)))
  }
| fint_lit as i {
    LITERAL(VFixedInt(Int64.of_string i))
  }
| int_lit as i {
    LITERAL(VInt(Big_int.big_int_of_string @@ strip_mod i))
  }
| ufint_lit as i {
    LITERAL(VUFixedInt(Uint64.of_string
                         (i |> strip_sign |> strip_mod)))
  }
| atom_lit as i {
    LITERAL(VAtom(Uint64.of_string (i |> strip_mod)))
  }
| float_lit as f {
    LITERAL(VFloat(float_of_string f))
  }
