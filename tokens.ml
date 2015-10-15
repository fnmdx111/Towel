open Common
open Parser
open Exc

let _ =
  let lexbuf = Lexing.from_channel stdin in
  try
    let rec accum acc =
      let token = Scanner.token lexbuf in
      (match token with
         LITERAL(l) -> accum ("some literal"::acc)
       | NAME(n) -> accum ((name_stringify n)::acc)
       | BQUOTE -> accum ("`"::acc)
       | COMMA -> accum ("COMMA,"::acc)
       | SEMICOLON -> accum (";"::acc)
       | TERMINATOR(_) -> "."::acc
       | LPAREN -> accum ("("::acc)
       | RPAREN -> accum (")"::acc)
       | LBRACKET -> accum ("["::acc)
       | RBRACKET -> accum ("]"::acc)
       | SLASH -> accum ("\\"::acc)
       | AT -> accum ("@"::acc)
       | LBRACE -> accum ("{"::acc)
       | RBRACE -> accum ("}"::acc)
       | BIND -> accum ("bind"::acc)
       | IFT -> accum ("ift"::acc)
       | FUNCTION -> accum ("fun"::acc)
       | _ -> accum (""::acc))
      in let xs = accum [] in List.iter print_endline (List.rev xs)
  with LexicalError(s, ln, b) ->
    Printf.printf "(%d,%d) Lexical error: %s.\n" ln b s
