open Tasm_ast;;
open T;;
open Stdint;;
open Dscoping;;
open Nstack;;

type ctx_t = {mod_id: uint64; ret_addr: line_no_t; curfun: value_t};;
type module_t = {id: uint64; insts: line array; exs: scope_t;
                 scps: scope_t list ref;
                 dss: value_t dstack dstack};;

(* In tail recursive calls, push-tail-name jumps two more lines,
   also fun-arg fetches from current data stack rather than parent
   data stack. *)
type flags_t = {is_tail_recursive_call: bool;
                is_main: bool; is_init_ext_mod: bool;
                import_stack: int list; (* 1 for implicit; 2 for explicit;
                                           0 for no *)
                list_make_stack: vidx_t list; tuple_make_stack: vidx_t list;
                curmod: module_t};;

type module_table_t = (uint64, module_t) Hashtbl.t;;
