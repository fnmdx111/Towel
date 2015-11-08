open Ast

(* ==========================================
   Scoping utilities for both Towel compiler and virtual machine

   + A Towel module consists of different scopes, and as functions
     being invoked, new scopes along with the functions are pushed/popped
     onto the scope stack.
   + A scope is randomized a hashtable (because it's mutable in OCaml),
     basic operations consists of adding new names (including overwriting),
     looking up names, and maybe some metaprogramming infrastructures.
   ========================================== *)

let is_DEBUG = ref true;

type scope = Scope of (string, int64) Hashtbl.t * string;;

let name_of_scope = function
    Scope(_, n) -> n;;

let table_of_scope = function
    Scope(h, _) -> h;;

let init_scope_stack = [];;

let push_scope ?ctx_name:(c="unnamed ctx") scp_stk =
  Scope(Hashtbl.create ~random:true 512, c)::scp_stk;;

let pop_scope scp_stk =
  match scp_stk with
    _::rest -> rest
  | [] -> [];;

let push_name scp_stk name value =
  Hashtbl.replace (table_of_scope (List.hd scp_stk)) name.name_repr value;;

let print_ht ht = print_string "ht:\n";
    Hashtbl.iter (fun k v -> Printf.printf "%s -> %s\n" k (Int64.to_string v)) ht;
    print_string "-----\n";;

let print_scp_stk scp_stk = print_string "scope stack:\n";
    List.iter (fun x -> match x with Scope(h, _) -> print_ht h) scp_stk;
  print_string "_____\n";;

let pop_name scp_stk name =
  Hashtbl.remove (table_of_scope (List.hd scp_stk)) name.name_repr;;

let rec lookup_name scp_stk name =
  match scp_stk with
    [] -> if !is_DEBUG then -1L
    else
    raise (Exc.NameNotFoundError
                   (Printf.sprintf
                      "requested name `%s' not found" name.name_repr))
  | scp::rest ->
    try
      let vs = Hashtbl.find_all (table_of_scope scp) name.name_repr in
      match vs with
        [] -> lookup_name rest name
      | v::[] -> v
      | v::vs -> raise (Exc.CorruptedScope
                          (Printf.sprintf "scope %s is corrupted"
                           (name_of_scope scp)))
    with Not_found -> lookup_name rest name
