open Batteries;;

type switches =
    CompilerSwitches of bool * (* hungry *)
                        bool * (* share stack *)
                        bool;; (* opt seq *)

let sw_hungry_if = function CompilerSwitches(b, _, _) -> b;;
let sw_share_stack = function CompilerSwitches(_, b, _) -> b;;
let sw_opt_seq = function CompilerSwitches(_, _, b) -> b;;

let _default = CompilerSwitches(false, false, true);;

let parse src =
  let in_ = IO.input_string src

  in let first_line = input_line in_
  in let second_line = input_line in_

  in close_in in_;
  if second_line <> "\n"
  then _default, false
  else let hungry = BatString.exists first_line "hungry"
    in let share_stack = BatString.exists first_line "share-stack"
    in let opt_seq = not @@ BatString.exists first_line "optimize-seq"
    in CompilerSwitches(hungry, share_stack, opt_seq),
       if not hungry && not share_stack && opt_seq
       then false
       (* None of the switches appeared in the preamble, in this situation, *)
       (* compiler reads the source from the very beginning. *)
       else true

