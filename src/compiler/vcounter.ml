
type vcounter = VCount of string * (* vcounter name *)
                          int64 * (* function count *)
                          int64 * (* sequence count *)
                          int64 * (* if count *)
                          int64;; (* match count *)
(* Note that this assumes every Towel source file creates less than
   2147483648 / 2 - 1 functions, sequences, if and match
   expressions. I ..think.. it's enough. *)

let vc_zero fn = VCount(fn, Int64.zero, Int64.zero, Int64.zero, Int64.zero);;

let vc_fun_inc c = match c with
  VCount(fn, i, j, k, l) -> VCount(fn, Int64.succ i, j, k, l);;

let vc_seq_inc c = match c with
  VCount(fn, i, j, k, l) -> VCount(fn, i, Int64.succ j, k, l);;

let vc_if_inc c = match c with
  VCount(fn, i, j, k, l) -> VCount(fn, i, j, Int64.succ k, l);;

let vc_match_inc c = match c with
  VCount(fn, i, j, k, l) -> VCount(fn, i, j, k, Int64.succ l);;

let vc_fun_dec c = match c with
  VCount(fn, i, j, k, l) -> VCount(fn, Int64.pred i, j, k, l);;

let vc_seq_dec c = match c with
  VCount(fn, i, j, k, l) -> VCount(fn, i, Int64.pred j, k, l);;

let vc_if_dec c = match c with
  VCount(fn, i, j, k, l) -> VCount(fn, i, j, Int64.pred k, l);;

let vc_match_dec c = match c with
  VCount(fn, i, j, k, l) -> VCount(fn, i, j, k, Int64.pred l);;

let vc_fun c = match c with
  VCount(_, i, _, _, _) -> i;;

let vc_seq c = match c with
  VCount(_, _, j, _, _) -> j;;

let vc_if c = match c with
  VCount(_, _, _, k, _) -> k;;

let vc_match c = match c with
  VCount(_, _, _, _, l) -> l;;

let _sfun i = if Int64.compare i Int64.zero > 0
  then Printf.sprintf "fun%s" @@ Int64.to_string i
  else "";;

let _sseq i = if Int64.compare i Int64.zero > 0
  then Printf.sprintf "seq%s" @@ Int64.to_string i
  else "";;

let _sif i = if Int64.compare i Int64.zero > 0
  then Printf.sprintf "if%s" @@ Int64.to_string i
  else "";;

let _smatch i = if Int64.compare i Int64.zero > 0
  then Printf.sprintf "match%s" @@ Int64.to_string i
  else "";;

let vc_to_label = function
    VCount(fn, i, j, k, l) -> Printf.sprintf ":%s-%s%s%s%s" fn
                            (_sfun i) (_sseq j) (_sif k) (_smatch l);;

