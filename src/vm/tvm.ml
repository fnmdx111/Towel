open Tasm_ast;;
open Batteries;;
open Vm;;

let src_file_r = ref "";;
let trace_r = ref false;;
let no_warnings_r = ref false;;

let commands = [
  ("-t", Arg.Set(trace_r), "Trace the flow of the instructions");
  ("-nw", Arg.Set(no_warnings_r), "Don't display warnings");
];;

let () = Arg.parse commands (fun fn -> src_file_r := fn)
    "Don't panic!";;

let src_file = !src_file_r;;
let src_inchan = Pervasives.open_in src_file;;
let src_content = String.concat "\n" @@ input_list src_inchan;;
Pervasives.close_in src_inchan;;

let () =
  let lexbuf = Lexing.from_string src_content
  in let text = Tasm_parser.asm Tasm_scanner.token lexbuf
  in exec !trace_r (not !no_warnings_r)
    (Array.of_list (match text with Asm(ins) -> ins));;

