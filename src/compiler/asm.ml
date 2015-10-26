open Common

let _ =
  let lexbuf = Lexing.from_channel stdin in
  try
    let ast = Parser.sentence Scanner.token lexbuf in
    let result =
      let 
