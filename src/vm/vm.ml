open T;;
open Stdint;;
open Tasm_ast;;
open Ort;;

type ctx_t = {id: uint64; ret_addr: uint64};;
type module_t = {ort: ort_t; exs: scope};;

type func_call_type = RegularCall | TailRecursiveCall;;
(* In tail recursive calls, push-tail-name jumps two more lines,
   also fun-arg fetches from current data stack rather than parent
   data stack. *)

let current_module = {ort = make_ort; exs = Hashtbl.create 512};;
let modules:(uint64, module_t) Hashtbl.t = Hashtbl.create 512;;

let dss_ = [];;
let scps_ = [];;
let ctxs_ = [];;

let tos = List.hd;;
let ntos = List.tl;;

let exec insts =
  let rec __exec ctxs dss scps ip =
    let push_todss x = x::(tos dss)::(ntos dss)
    in let next_ip = succ ip
    in let inst = insts.(ip)
    in match inst with
      PUSH_INT(ArgLit(VInt(i))) ->
      let nr = new_int current_module.ort i
      in __exec ctxs (push_todss nr) scps next_ip
    | JUMP(ArgLit(VUFixedInt(p))) ->
      __exec ctxs dss scps p
    |
      (* !!! *)
  in __exec ctxs_ dss_ scps_ 0
