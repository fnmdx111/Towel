
type vcounter = VCount of string * (* vcounter name *)
                          int * (* function count *)
                          int * (* sequence count *)
                          int * (* if count *)
                          int;; (* match count *)
(* Note that this assumes every Towel source file creates less than
   2147483648 / 2 - 1 functions, sequences, if and match
   expressions. I ..think.. it's enough. *)

let vc_zero fn = VCount(fn, 0, 0, 0, 0);;

let vc_fun_inc c = match c with
  VCount(fn, i, j, k, l) -> VCount(fn, i + 1, j, k, l);;

let vc_seq_inc c = match c with
  VCount(fn, i, j, k, l) -> VCount(fn, i, j + 1, k, l);;

let vc_if_inc c = match c with
  VCount(fn, i, j, k, l) -> VCount(fn, i, j, k + 1, l);;

let vc_match_inc c = match c with
  VCount(fn, i, j, k, l) -> VCount(fn, i, j, k, l + 1);;

let vc_fun_dec c = match c with
  VCount(fn, i, j, k, l) -> VCount(fn, i - 1, j, k, l);;

let vc_seq_dec c = match c with
  VCount(fn, i, j, k, l) -> VCount(fn, i, j - 1, k, l);;

let vc_if_dec c = match c with
  VCount(fn, i, j, k, l) -> VCount(fn, i, j, k - 1, l);;

let vc_match_dec c = match c with
  VCount(fn, i, j, k, l) -> VCount(fn, i, j, k, l - 1);;

let vc_fun c = match c with
  VCount(_, i, _, _, _) -> i;;

let vc_seq c = match c with
  VCount(_, _, j, _, _) -> j;;

let vc_if c = match c with
  VCount(_, _, _, k, _) -> k;;

let vc_match c = match c with
  VCount(_, _, _, _, l) -> l;;

let _sfun i = if i > 0
  then Printf.sprintf "fun%d" i
  else "";;

let _sseq i = if i > 0
  then Printf.sprintf "seq%d" i
  else "";;

let _sif i = if i > 0
  then Printf.sprintf "if%d" i
  else "";;

let _smatch i = if i > 0
  then Printf.sprintf "match%d" i
  else "";;

let vc_to_label = function
    VCount(fn, i, j, k, l) -> Printf.sprintf ":%s-%s%s%s%s" fn
                            (_sfun i) (_sseq j) (_sif k) (_smatch l);;

