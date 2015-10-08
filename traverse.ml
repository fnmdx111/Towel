open Ast
open Common

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let ast = Parser.sentence Scanner.token lexbuf in
  let result =
    let traverse x =
      match x with
        Sentence(words, _) ->
        words_stringify words
    in traverse ast
  in print_endline result
