open Batteries;;
open Exc;;

let src_file_r = ref "";;
let in_files_r = ref [];;
let out_file_r = ref "";;
let raw_asm_r = ref false;;

let commands = [
    ("-o",
     Arg.Set_string(out_file_r),
     "Path of the output file");

    ("-i",
     Arg.Rest(fun _s -> in_files_r := _s::(!in_files_r)),
     "Paths of input files");

    ("-r",
     Arg.Set(raw_asm_r),
     "Raw asm with their labels on");
  ];;

let () = Arg.parse commands (fun fn -> src_file_r := fn)
       "The Towel Compiler at your service. Don't panic!";;

let raw_asm = !raw_asm_r;;

let src_file = !src_file_r;;
let src_inchan = Pervasives.open_in src_file;;
let src_content = String.concat "\n" @@ input_list src_inchan;;

Pervasives.close_in src_inchan;;

let in_files = List.rev !in_files_r;;
let out_file = !out_file_r;;

let in_chans = List.map (fun fn -> Pervasives.open_in fn, fn) in_files;;
let in_srcs = List.map (fun (inc, fn) -> input_list inc, fn) in_chans;;

List.iter Pervasives.close_in @@ List.map fst in_chans;;

let asm =
  let in_src, fn = src_content, src_file

  in let sw, has_sw_preamble = Switches.parse(in_src)
  in let text = if has_sw_preamble
       then let _, r = String.split src_content "\n"
         in let _, r = String.split r "\n"
         in r
       (* ignore the two-line preamble so that no syntax error will show up *)
       else src_content

  in let lexbuf = Lexing.from_string text
  in try

    let cst = Parser.sentence Scanner.token lexbuf

    in let fn_digest = src_content
                       |> Sha1.string
                       |> Sha1.to_hex
                       |> fun x -> String.sub x 0 9

    in if (* type checking *) true
    then Asm.assemble cst fn_digest sw
    else raise TypeError

  with
  | LexicalError(s, ln, b) ->
    Printf.printf "(%d,%d) Lexical error: %s.\n" ln b s; exit 0
  | SyntacticError(s, ln, st, e) ->
    Printf.printf "(%d,%d-%d) Syntactic error: %s.\n"
      ln st e s; exit 0
  | NameNotFoundError(s) ->
    Printf.printf "Name not found: %s.\n" s; exit 0
  | TypeError ->
    Printf.printf "type error\n"; exit 0

in let text =
     if raw_asm then asm
     else let lexbuf = Lexing.from_string asm
       in let cst = Tasm_parser.asm Tasm_scanner.token lexbuf
       in cst
          |> Unlabel.unlabel
          |> Tasm_stringify.p_asm

in let ochan =
     if out_file = "-"
     then Pervasives.stdout
     else Pervasives.open_out out_file
in Pervasives.output_string ochan text; Pervasives.flush ochan;
if out_file = "-"
then () else Pervasives.close_out ochan
