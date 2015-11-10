open T;;

(* ==========================================
   Scoping utilities for the virtual machine
   ========================================== *)

let is_DEBUG = ref true;;

type scope = Scope of (name_t, ref_t) Hashtbl.t;;

let t_of_scope = function Scope(h) -> h;;

let push_scope scp_stk =
  Scope(Hashtbl.create ~random:true 512)::scp_stk;;

let pop_scope =
  function
    _::rest -> rest
  | [] -> [];;

let push_name scp_stk name value =
  Hashtbl.replace (t_of_scope (List.hd scp_stk)) name value;;

let pop_name scp_stk name =
  Hashtbl.remove (t_of_scope (List.hd scp_stk)) name;;

let rec lookup_name scp_stk name =
  match scp_stk with
    [] -> if !is_DEBUG then -1L
    else raise (Exc.NameNotFoundError name)
  | scp::rest ->
    try let vs = Hashtbl.find_all (table_of_scope scp) name
      in match vs with
        [] -> lookup_name rest name
      | v::[] -> v
      | v::vs -> raise (Exc.CorruptedScope)
    with Not_found -> lookup_name rest name;;

