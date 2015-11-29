open Stdint;;
open Tasm_ast;;
open Dscoping;;

(* ==========================================
     Woven file parser
   ========================================== *)

let wvn_filename fn =
  String.concat "." [fn; "w"];;

let open_woven path =
  let lexbuf = Lexing.from_channel @@ Pervasives.open_in path
  in Array.of_list
         (match Tasm_parser.asm Tasm_scanner.token lexbuf with
         Asm(ins) -> ins);;
