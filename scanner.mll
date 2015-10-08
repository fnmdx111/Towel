{
open Ast
open Parser
open Common
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
let _FTO = "->"

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
| _BQUOTE { BQUOTE }
| _COMMA { COMMA }
| _SEMICOLON { SEMICOLON }
| _PERIOD { TERMINATOR(Ast.Period) }
| _LPAREN { LPAREN }
| _RPAREN { RPAREN }
| _LBRACKET { LBRACKET }
| _RBRACKET { RBRACKET }
| _SLASH { SLASH }
| _FTO { FTO }

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

| name as n {
    NAME({name_ref_key = name_counter ();
          name_repr = n;
          name_type = TypeDef([TDPrimitiveType(PT_Any)])})
  }

              (* literals start here *)
| atom_lit as a {
    LITERAL({value_id = value_counter ();
             value_content = VAtom({atom_name = a;
                                    atom_repr = value_counter ()});
             value_type = TypeDef([TDPrimitiveType(PT_Atom)])})
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
    LITERAL({value_id = -1;
             value_content = VString("not matched " ^ (String.make 1 s));
             value_type = TypeDef([TDPrimitiveType(PT_String)])})
  }

| _DQUOTE [^ '"'] _DQUOTE { token lexbuf } (* comments *)
