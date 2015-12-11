open Batteries
open Ast
open Stdint

(* =======================================
     Counter
   ======================================= *)
let counter = fun () ->
  let cnt = Array.of_list [Uint64.of_int 0]
  in fun () -> cnt.(0) <- Uint64.succ cnt.(0); cnt.(0);;
(* Losing one -1 for a whole bunch of 2**63. *)

let tu64 x = Printf.sprintf "%su" @@ Uint64.to_string x;;

(* =======================================
     AST stringifiers
   ======================================= *)
module P = Printf;;

let rec atom_stringify a = P.sprintf "(atom %s %d)" a.atom_name a.atom_repr
and pname_stringify pn = P.sprintf "(name %s)" pn.name_repr
and name_stringify name = let a = match name with
      NRegular(ns) -> ns
    | NTailCall(ns) -> ns
  in String.concat " of " (List.map pname_stringify a)
and int_stringify a = P.sprintf "(int-lit %s)" @@ Int64.to_string a
and float_stringify a = P.sprintf "(float-lit %f)" a
and string_stringify a = P.sprintf "(str-lit %s)" a
and tuple_stringify a = P.sprintf "(tuple-lit %s)"
    (String.concat ", " (List.map word_stringify a))
and lit_stringify = function
  VAtom(a) -> atom_stringify a
| VFixedInt(i) -> int_stringify i
| VFloat(f) -> float_stringify f
| VList(l) -> list_stringify l
| VString(ss) -> string_stringify ss
| VTuple(ws) -> tuple_stringify ws
| _ -> "i'm just too lazy"

and seq_stringify seq =
  match seq with
    Sequence(ws) -> String.concat ", " (List.map word_stringify ws)
  | SharedSequence(ws) -> String.concat "-, " (List.map word_stringify ws)

and backquote_stringify a = P.sprintf "(bq-lit %s)"
    (match a with
       BQValue(pv) -> lit_stringify pv.value_content
     | BQName(n) -> name_stringify n
     | BQSeq(seq) -> seq_stringify seq
     | BQBackquote(bq) -> backquote_stringify bq)

and cs_stringify cs =
  let if_stringify s body =
    match body with
      IfBody(w1, w2) -> P.sprintf "%s { %s; %s }" s
                          (word_stringify w1)
                          (word_stringify w2)
  in match cs with
    CtrlSeqIfForm(i) ->
    (match i with
       IfGEZ(ib) -> if_stringify "gez" ib
     | IfGZ(ib) -> if_stringify "gz" ib
     | IfLEZ(ib) -> if_stringify "lez" ib
     | IfLZ(ib) -> if_stringify "lz" ib
     | IfEmpty(ib) -> if_stringify "empty" ib
     | IfNonEmpty(ib) -> if_stringify "non-empty" ib
     | IfEZ(ib) -> if_stringify "ez" ib
     | IfNEZ(ib) -> if_stringify "nez" ib
     | IfT(ib) -> if_stringify "t" ib
     | IfF(ib) -> if_stringify "f" ib)

and arg_def_stringify d =
    match d with
      ArgDef(n) -> pname_stringify n

and bind_body_stringify = function
    BindBody(n, w) ->
    P.sprintf "%s = %s" (pname_stringify n) (word_stringify w)

and bind_stringify = function
    BindThen(bodies, w) ->
    P.sprintf "%s in %s"
      (String.concat " also "
         (List.map bind_body_stringify bodies))
      (word_stringify w)

and fun_stringify = function
    BQFunction(ds, w)
  | Function(ds, w) ->
    P.sprintf "fun %s = %s"
      (String.concat "; " (List.map arg_def_stringify ds))
      (word_stringify w)

and word_stringify w =
  let _w s n = P.sprintf "(%s of %s)" s n in
  match w with
    WLiteral(pv) -> _w (lit_stringify pv.value_content) "literal"
  | WName(n) -> _w (name_stringify n) "name"
  | WBackquote(bq) -> _w (backquote_stringify bq) "bquote"
  | WSequence(seq) -> _w (seq_stringify seq) "seq"
  | WControl(cs) -> _w (cs_stringify cs) "cs"
  | WFunction(f) -> _w (fun_stringify f) "fun"
  | WBind(b) -> _w (bind_stringify b) "bind"
  | WImport(is) -> "some imports"
  | WExport(ns) -> "some exports"

and words_stringify ws =
  String.concat "/" (List.map word_stringify ws)

and list_stringify lss =
  let rec _strf s ls =
    match ls with
      [] -> s
    | w::ws ->
      _strf (String.concat " | " [(word_stringify w); s]) ws
  in _strf "" lss;;
