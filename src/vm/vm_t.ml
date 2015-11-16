open Tasm_ast;;
open T;;
open Stdint;;
open Scoping;;
open Ort;;

type ctx_t = {id: uint64; ret_addr: uint64};;
type module_t = {insts: line array; exs: scope};;

(* In tail recursive calls, push-tail-name jumps two more lines,
   also fun-arg fetches from current data stack rather than parent
   data stack. *)
type flags_t = {is_tail_recursive_call: bool;
                is_main: bool; is_init_ext_mod: bool;
                is_import: int; (* 1 for implicit; 2 for explicit; 0 for no *)
                module_id: uint64; current_module: module_t};;

type module_table_t = (uint64, module_t) Hashtbl.t;;

