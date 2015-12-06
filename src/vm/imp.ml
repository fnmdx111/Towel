open Stdint;;
open Tasm_ast;;
open Dscoping;;

(* ==========================================
     Woven file parser
   ========================================== *)

let wvn_filename fn =
  String.concat "." [fn; "w"];;

let open_woven path =
  Tasm_inv_bytecode.parse_bytecode (Pervasives.open_in path)

