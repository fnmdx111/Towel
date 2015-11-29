open Tasm_ast;;
open T;;
open Stdint;;
open Dscoping;;
open Nstack;;

type ctx_t = {mod_id: module_id_t; ret_addr: line_no_t; curfun: value_t};;
type module_t = {id: module_id_t; insts: line array;
                 exs: (name_t, value_t) Hashtbl.t;
                 imports: (module_id_t, module_id_t) Hashtbl.t;
                 (* Map from relative module id to absolute module id. *)
                 scps: scope_t list ref}

let print_module m =
  Printf.fprintf stderr "Module {
  id = %s; insts = <insts>; exs = %s;
  scps: %s
}" (Uint64.to_string m.id) "<exs>"
    (sprint_dscope_stack !(m.scps));;

(* In tail recursive calls, eval-tail jumps two more lines,
   also fun-arg fetches from current data stack rather than parent
   data stack. *)
type flags_t = {is_tail_recursive_call: bool;
                is_main: bool; is_init_ext_mod: bool;
                import_stack: int list; (* 1 for implicit; 2 for explicit;
                                           0 for no *)
                list_make_stack: vidx_t list; tuple_make_stack: vidx_t list;
                curmod: module_t;
                dss: value_t dstack_t dstack_t;
                is_stepping: bool};;

type module_table_t = (uint64, module_t) Hashtbl.t;;
