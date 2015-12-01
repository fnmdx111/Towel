open Batteries;;
open T;;
open Printf;;
open Stdint;;

(* ==========================================
     Immperative array-based stack.
     The size of it is dynamically allocated.
   ========================================== *)


(* Only for debugging purposes. *)
let rec string_of_value v =
  match v with
    OVInt(i) -> Big_int.string_of_big_int i
  | OVFixedInt(i) -> Int64.to_string i
  | OVUFixedInt(i) -> Uint64.to_string i
  | OVFloat(f) -> string_of_float f
  | OVAtom(a) -> Uint64.to_string a
  | OVString(s) -> s
  | OVList(rs) ->
    sprintf "[%s]"
      (String.concat " " (List.map string_of_value !rs))
  | OVTuple(rs) ->
    sprintf "[@ %s]"
      (String.concat " " (List.map string_of_value !rs))
  | OVFunction(f) ->
    sprintf "**%s: %d,%s,<closure set>"
      (if f.is_partial then "partial-fun" else "fun")
      f.st (Uint64.to_string f.mod_id)
  | _ -> "**abstract value";;

type 'a dstack_t = 'a BatDynArray.t;;
exception PhonyEmptyStack;;

let dinit () = BatDynArray.make 128;;

let dsp ds = BatDynArray.length ds - 1;;

let dpush ds v =
  BatDynArray.add ds v;;

let _safe_dsp ds = pred (dsp ds);;

let dtop ds =
  let top = BatDynArray.last ds
  in if top = OVPhony
  then raise PhonyEmptyStack
  else top;;

let dis_empty ds = if BatDynArray.length ds = 0
    then 1
    else if dtop ds = OVPhony
    then 2
    else 0;;

let dswap ds idx1 idx2 =
  let t = BatDynArray.get ds idx1
  in BatDynArray.set ds idx1 (BatDynArray.get ds idx2);
  BatDynArray.set ds idx2 t;;

let dpop ds =
  let popped = dtop ds
  in begin
    BatDynArray.delete_last ds;
    if popped = OVPhony
    then raise PhonyEmptyStack
    else popped
  end;;

let dpurge = BatDynArray.clear;;

let dspush dss v =
  let stack = BatDynArray.last dss
  in dpush stack v;;

let dstop dss =
  let stack = BatDynArray.last dss
  in dtop stack;;

let dspop dss =
  let stack = BatDynArray.last dss
  in dpop stack;;

let dsis_empty dss =
  let stack = BatDynArray.last dss
  in dis_empty stack;;

let dspurge (* purge the top stack *) dss =
  let stack = BatDynArray.last dss
  in begin
    BatDynArray.delete_last dss;
    dpurge stack
  end;; (* Maybe an overkill? Maybe not. *)

let sprint_ds ds = Printf.sprintf "[|%s|]"
    (String.concat ", "
       (BatDynArray.fold_left
          (fun acc x -> (string_of_value x)::acc)
          [] ds));;

let sprint_dss dss = Printf.sprintf "[[| %s |]]"
    (String.concat " | "
       (BatDynArray.fold_left
          (fun acc x -> (sprint_ds x)::acc)
          [] dss));;

(* ==========================================
     Below are some indexing functions.
   ========================================== *)

type vidx_t = int * int;; (* ds idx, dss idx *)

let dval dss idx =
  let ds = BatDynArray.get dss (snd idx)
  in BatDynArray.get ds (fst idx);;

let snd_ds dss = BatDynArray.get dss ((_safe_dsp dss));;
