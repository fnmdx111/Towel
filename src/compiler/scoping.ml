open Ast;;
open Stdint;;

(* ==========================================
   Scoping utilities for both Towel compiler

   + A Towel module consists of different scopes, and as functions
     being invoked, new scopes along with the functions are pushed/popped
     onto the scope stack.
   + A scope is randomized a hashtable (because it's mutable in OCaml),
     basic operations consists of adding new names (including overwriting),
     looking up names, and maybe some metaprogramming infrastructures.
   ========================================== *)

let is_DEBUG = ref false;;

type name_t = uint64;;
type scope_t = (string, name_t) Hashtbl.t;;

type external_scope_t = ExtScope of (string, external_scope_t) Hashtbl.t (* path table *)
                                    * (string, name_t) Hashtbl.t       (* scope *)
                                    * uint64                           (* id *)

let print_ht ht = print_string "ht:\n";
    Hashtbl.iter (fun k v -> Printf.printf "%s -> %s\n" k (Uint64.to_string v)) ht;
  print_string "-----\n";;

let print_es es = print_string "es:\n";
  Hashtbl.iter (fun k v -> Printf.printf "%s -> %s\n" k "es")
    (match es with ExtScope(p, _, _) -> p);
  print_ht (match es with ExtScope(_, p, _) -> p);
  print_string "-----\n";;

let push_ext_scope meta ext_scope uid possible_mod_path =
  let rec _push_e_s cur mod_path =
    try let parent, rest = BatString.split Filename.dir_sep mod_path
      in match cur with
        ExtScope(package_path, _, _) ->
        if Hashtbl.mem package_path parent
        then _push_e_s (Hashtbl.find package_path parent) rest
        else Hashtbl.replace package_path (BatString.capitalize parent)
            (ExtScope(Hashtbl.create 512, Hashtbl.create 512, Uint64.zero));
        _push_e_s (Hashtbl.find package_path parent) rest

    with Not_found ->
    match cur with
    ExtScope(paths, _, _) ->
      Hashtbl.replace paths (BatString.capitalize mod_path)
        (ExtScope(Hashtbl.create 512, ext_scope, uid))
  in _push_e_s meta possible_mod_path;;

let lookup_ext_name meta ns =
  let rec _lookup cur = function
      [] -> failwith "Not found."
    | n::[] -> (match cur with
          ExtScope(_, ext_scope, uid) -> Hashtbl.find ext_scope n.name_repr, uid)
    | name::rest -> (match cur with
          ExtScope(p, _, _) -> _lookup (Hashtbl.find p name.name_repr) rest)

  in _lookup meta (List.rev ns)

let init_scope_stack = [];;

let push_scope ?ctx_name:(c="unnamed ctx") scp_stk =
  (Hashtbl.create ~random:true 512)::scp_stk;;

let pop_scope scp_stk =
  match scp_stk with
    _::rest -> rest
  | [] -> [];;

let push_name scp_stk name value =
  Hashtbl.replace (List.hd scp_stk) name.name_repr value;;

let print_scp_stk scp_stk = print_string "scope stack:\n";
    List.iter (fun h -> print_ht h) scp_stk;
  print_string "_____\n";;

let pop_name scp_stk name =
  Hashtbl.remove (List.hd scp_stk) name.name_repr;;

let rec lookup_name scp_stk name =
  match scp_stk with
    [] -> if !is_DEBUG (* culture shock!! *)
    then Uint64.zero
    else raise (Exc.NameNotFoundError
                  (Printf.sprintf
                     "requested name `%s' not found" name.name_repr))
  | scp::rest ->
    try
      let vs = Hashtbl.find_all scp name.name_repr in
      match vs with
        [] -> lookup_name rest name
      | v::[] -> v
      | v::vs -> raise (Exc.CorruptedScope
                          (Printf.sprintf "scope %s is corrupted"
                           "some scope"))
    with Not_found -> lookup_name rest name
