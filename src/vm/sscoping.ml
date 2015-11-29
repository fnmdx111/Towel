open Nstack;;

type scope = ();;

let push_scope scp_stk = scp_stk;;
let pop_scope scp_stk = scp_stk;;

let npush scp_stk name idx = ();;

let npop scp_stk name = ();;

let rec lookup_name scp_stk name = ();;
(* Don't think this will be used. *)
