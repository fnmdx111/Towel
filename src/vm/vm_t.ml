open Tasm_ast;;
open T;;
open Stdint;;
open Dscoping;;
open Nstack;;

type ctx_t = {mod_id: module_id_t; ret_addr: line_no_t; curfun: fun_t;
              scp_count: int;
              is_tail_recursive_call: bool;
              list_make_stack: value_t list ref list};;
type module_t = {id: module_id_t; insts: line array;
                 exs: (name_t, value_t) Hashtbl.t;
                 imports: (module_id_t, module_id_t) Hashtbl.t;
                 name_id_tick: (name_t -> name_t);
                 (* Map from relative module id to absolute module id. *)}


let print_module m =
  Printf.fprintf stderr "Module {
  id = %d; insts = <insts>; exs = %s;
}" m.id "<exs>";;


(* In tail recursive calls, eval-tail jumps two more lines,
   also fun-arg fetches from current data stack rather than parent
   data stack. *)
type flags_t = {curmod: module_t;
                dss: value_t dstack_t dstack_t;
                scps: scope_t list;
                is_stepping: bool};;

type module_table_t = (module_id_t, module_t) Hashtbl.t;;
