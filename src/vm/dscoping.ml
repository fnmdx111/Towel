open Nstack;;
open T;;

(* ==========================================
     Scoping routines for Towel virtual machine.
     Special designed for both dynamic and static flavors.

     We'll use the dynamic one for now though. For static scoping,
   see sscoping.ml.

     If I were to enable users to add names at runtime, I'll be adding
   code here.
   ========================================== *)

type scope_t = (name_t, vidx_t) Hashtbl.t;;

let push_scope scp_stk = (Hashtbl.create 512)::scp_stk;;

let pop_scope = function _::rest -> rest | [] -> [];;

let npush scp_stk name idx =
  Hashtbl.replace (List.hd scp_stk) name idx;;

let npop scp_stk name =
  Hashtbl.remove (List.hd scp_stk) name;;

let rec nlookup scp_stk name =
  match scp_stk with
    [] -> raise (Exc.NameNotFoundError name)
  | scp::rest ->
    try Hashtbl.find scp name
    with Not_found -> nlookup rest name;;

let sprint_dscope dscope =
  Printf.sprintf "{%s}"
    (String.concat ", "
       (Hashtbl.fold (fun k v acc -> (Printf.sprintf "%d: (%d,%d)" k
                         (fst v) (snd v))::acc) dscope []));;

let sprint_dscope_stack scp_stk =
  String.concat "; " (List.map (fun x -> sprint_dscope x) scp_stk);;
