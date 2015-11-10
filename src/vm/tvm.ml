
open Tasm_ast;;

let src_file_r = ref "";;

let commands = [];;

let () = Arg.parse commands (fun fn -> src_file_r := fn)
    "Don't panic!";;

let src_file = !src_file_r;;
let src_inchan = open_in src_file;;
let src_content = String.concat "\n" @@ input_list src_inchan;;
close_in src_inchan;;


let text =
  let lexbuf = Lexing.from_string src_content
  in let cst = Tasm_parser.asm Tasm_scanner.token lexbuf
  in exec text;;

