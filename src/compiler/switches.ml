
type switches =
    CompilerSwitches of bool * (* hungry if *)
                        bool * (* share stack *)
                        bool;; (* opt seq *)

let sw_hungry_if = function CompilerSwitches(b, _, _) -> b;;
let sw_share_stack = function CompilerSwitches(_, b, _) -> b;;
let sw_opt_seq = function CompilerSwitches(_, _, b) -> b;;

