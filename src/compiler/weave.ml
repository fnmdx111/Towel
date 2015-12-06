open Batteries;;
open Exc;;
open Stdint;;

let src_file_r = ref "";;
let out_file_r = ref "";;
let raw_asm_r = ref false;;
let raw_text_r = ref false;;
let byte_compile_r = ref false;;

let commands = [
    ("-o",
     Arg.Set_string(out_file_r),
     "Path of the output file");

    ("-r",
     Arg.Set(raw_asm_r),
     "Raw asm with labels on");

    ("-t",
     Arg.Set(raw_text_r),
     "Raw text asm");

    ("-b",
     Arg.Set(byte_compile_r),
     "Byte-compile the given source");
  ];;

let () = Arg.parse commands (fun fn -> src_file_r := fn)
       "The Towel Compiler at your service. Don't panic!";;

let raw_asm = !raw_asm_r;;
let raw_text = !raw_text_r;;

let src_file = !src_file_r;;
let src_inchan = Pervasives.open_in src_file;;
let src_content = String.concat "\n" @@ input_list src_inchan;;

Pervasives.close_in src_inchan;;

let out_file = !out_file_r;;

let out = ref "";;

if !byte_compile_r
then let _out =
  let lexbuf = Lexing.from_string src_content
  in lexbuf
     |> Tasm_parser.asm Tasm_scanner.token |> Tasm_bytecode.b_asm
     |> Bytes.to_string
  in out := _out
else begin
  let fn_digest = src_content
                  |> Sha1.string
                  |> Sha1.to_hex
                  |> fun x -> String.sub x 0 9

  in let ir, exp_scope =
       let in_src, fn = src_content, src_file

       in let sw, has_sw_preamble = Switches.parse(in_src)
       in let text = if has_sw_preamble
            then let _, r = String.split src_content "\n"
              in let _, r = String.split r "\n"
              in r
              (* ignore the two-line preamble so that no syntax error
                 will show up *)
            else src_content

       in let lexbuf = Lexing.from_string text
       in try

         let cst = Parser.sentence Scanner.token lexbuf
         in if (* type checking *) true
         then Compile.compile cst fn_digest sw
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

  in out := if raw_asm
    then Tasm_stringify.p_asm ir
    else if raw_text
    then ir |> Assemble.assemble |> Tasm_stringify.p_asm
    else ir |> Assemble.assemble |> Tasm_bytecode.b_asm |> Bytes.to_string;
  let compose_exp_file x =
    Hashtbl.fold (fun nstr idx acc
                   -> acc ^ "\n"
                      ^ (Printf.sprintf "(%s %su)"
                           nstr (Uint64.to_string idx)))
      x ""
  in let content =
       Printf.sprintf "\"Automatically generated .e for %s.\"\n\n%s"
         fn_digest (compose_exp_file exp_scope)
  in let ochan =
       if out_file = "-"
       then Pervasives.stdout
       else Pervasives.open_out
         @@ String.concat "."
           [fst (String.rsplit out_file ".");
            "e"]
           (* Replace the t extension with e. *)
  in Pervasives.output_string ochan content; Pervasives.flush ochan;
  if out_file = "-"
  then ()
  else Pervasives.close_out ochan
end;;

let ochan =
     if out_file = "-"
     then Pervasives.stdout
     else Pervasives.open_out out_file
in Pervasives.output_string ochan (!out);
Pervasives.flush ochan;
if out_file = "-"
then () else Pervasives.close_out ochan;;

