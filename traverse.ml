open Common

let _ =
  let lexbuf = Lexing.from_channel stdin in
  try
    let ast = Parser.sentence Scanner.token lexbuf in
    let result =
      let traverse x =
        match x with
          Ast.Sentence(words, _) ->
          words_stringify words
      in traverse ast
    in print_endline result
  with
  | LexicalError(s, ln, b) ->
    Printf.printf "(%d,%d) Lexical error: %s.\n" ln b s
  | SyntacticError(s, ln, st, e) ->
    Printf.printf "(%d,%d-%d) Syntactic error: %s.\n"
      ln st e s

