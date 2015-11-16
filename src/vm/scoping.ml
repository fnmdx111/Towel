open T;;
open Stdint;;

(* ==========================================
   Scoping utilities for the virtual machine
   ========================================== *)

let is_DEBUG = ref false;;

type scope = (name_t, ref_t) Hashtbl.t;;

let push_scope scp_stk =
  (Hashtbl.create ~random:true 512)::scp_stk;;

let pop_scope =
  function
    _::rest -> rest
  | [] -> [];;

let push_name scp_stk name value =
  Hashtbl.replace (List.hd scp_stk) name value;;

let pop_name scp_stk name =
  Hashtbl.remove (List.hd scp_stk) name;;

let rec lookup_name scp_stk name =
  match scp_stk with
    [] -> if !is_DEBUG then Uint64.zero
    else raise (Exc.NameNotFoundError name)
  | scp::rest ->
    try Hashtbl.find scp name
    with Not_found -> lookup_name rest name;;

