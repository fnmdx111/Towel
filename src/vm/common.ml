open Batteries;;
open Stdint;;

(* ==========================================
     Counter
   ========================================== *)

let counter = fun () ->
  let cnt = Array.of_list [Uint64.zero]
  in fun () -> cnt.(0) <- Uint64.succ cnt.(0); cnt.(0);;
