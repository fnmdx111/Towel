open T;;
open Common;;
open Stdint;;

(* ==========================================
     Object reference table (per module)

     As a side note, each module has a object reference table and a scope of
   exported names.
   ========================================== *)

type ort_t = ObjRefTable of
    (unit -> uint64)                     (* the idx ticker *)
    * (ref_t, value_t) Hashtbl.t         (* the ort *)
    * (Big_int.big_int, ref_t) Hashtbl.t (* int pool *)
    * (int64, ref_t) Hashtbl.t           (* fint pool *)
    * (uint64, ref_t) Hashtbl.t          (* ufint pool *)
    * (uint64, ref_t) Hashtbl.t          (* atom pool *)
    * (string, ref_t) Hashtbl.t          (* string pool *)
    * (uint64, ref_t) Hashtbl.t          (* function pool *)
    * ref ref_t;;                        (* newestly created value ref *)

(* Ouch! I forgot I could use RECORD! *)

let new_ort = ObjRefTable(
    Common.counter (),
    Hashtbl.create 512,
    Hashtbl.create 512,
    Hashtbl.create 512,
    Hashtbl.create 512,
    Hashtbl.create 512,
    Hashtbl.create 512,
    Hashtbl.create 512,
    ref R(Uint64.zero));;

let ticker_of =
  function ObjRefTable(ticker, _, _, _, _, _, _, _, _) -> ticker;;

let ort_of =
  function ObjRefTable(_, ort, _, _, _, _, _, _, _) -> ort;;

let intp_of =
  function ObjRefTable(_, _, intp, _, _, _, _, _, _) -> intp;;

let fintp_of =
  function ObjRefTable(_, _, _, fintp, _, _, _, _, _) -> fintp;;

let ufintp_of =
  function ObjRefTable(_, _, _, _, ufintp, _, _, _, _) -> ufintp;;

let atomp_of =
  function ObjRefTable(_, _, _, _, _, atomp, _, _, _) -> atomp;;

let strp_of =
  function ObjRefTable(_, _, _, _, _, _, strp, _, _) -> strp;;

let funp_of =
  function ObjRefTable(_, _, _, _, _, _, _, funp, _) -> funp;;

let nref_of =
  function ObjRefTable(_, _, _, _, _, _, _, _, nref) -> !nref;;

let set_nref_of ort r =
  match ort with ObjRefTable(_, _, _, _, _, _, _, _, nref) ->
    nref := r;;

let lookup_val ort r =
  match ort with ObjRefTable(_, o, _, _, _, _, _, _, _) ->
    Hashtbl.find o r;;

let new_integer ort pool type_hint lit =
  try
    let r = Hashtbl.find pool lit
    in set_nref_of ort r; r
  with Not_found ->
    let ref_ = R((ticker_of ort) ())
    in set_nref_of ort ref_;
    Hashtbl.add pool lit ref_;
    Hashtbl.add (ort_of ort) ref_
      {v = (match type_hint with
             THInt -> OVInt(lit)
           | THFixedInt -> OVFixedInt(lit)
           | THUFixedInt -> OVUFixedInt(lit)
           | THAtom -> OVAtom(lit)
           | THFunction -> OVFunction(lit)
           | _ -> failwith "Incompatible type hint.");
       refc = Uint64.zero};
    ref_;;

let new_float ort lit = 
  let ref_ = R((ticker_of ort) ())   (*new added function new_float*)
in Hashtbl.add ort lit ref_;


let new_int ort = new_integer ort (intp_of ort) THInt;;
let new_fint ort = new_integer ort (fintp_of ort) THFixedInt;;
let new_ufint ort = new_integer ort (ufintp_of ort) THUFixedInt;;
let new_atom ort = new_integer ort (atomp_of ort) THAtom;;
let new_function ort = new_integer ort (funp_of ort) THFunction;;
(* Functions are just the start instruction numbers of UInt64. *)

let new_string ort lit =
  let ref_ =
    if String.length lit < 100
    then try Hashtbl.find (strp_of ort) lit
      with Not_found ->
        let _r = R((ticker_of ort) ())
        in Hashtbl.add (strp_of ort) lit _r; _r
        (* If the length of the string is less than 100, we will cache
           it in our string pool, and then return the ref. *)
    else R((ticker_of ort) ())
    (* Otherwise return a new ref directly. *)
  in set_nref_of ort ref_;
  (* No matter whether we found that string in the pool, we are going to
     set the newestly created value reference to it. Same for others. *)
  Hashtbl.add (ort_of ort) ref_ {v = VString(lit); refc = Uint64.zero};
  ref_;;

let new_list ort =
  let ref_ = R((ticker_of ort) ())
  (* We don't cache list values. *)
  in set_nref_of ort ref_; Hashtbl.find (ort_of ort) OVLNil;;

let new_tuple ort =
  let ref_ = R((ticker_of ort) ())
  (* Nor for tuples. *)
  in set_nref_of ort ref_; Hashtbl.find (ort_of ort) OVTNil;;

let _append_list_elem ort lref r =
  let nval = match lookpup_val lref with
      OVLNil -> OVList([nref])
    | OVList(rs) -> OVList(nref::rs)
  in Hashtbl.replace (ort_of ort) lref nval;;
(* This is to say, after appending the new item, we replace what was
   there with our new list. *)

let _new_list_item nfun ort lref lit =
  (* First, you make a new thing. *)
  let nref = nfun ort lit
  (* Then you can append it to the list. *)
  in _append_list_elem ort lref nref; nref;;

let new_list_int = _new_list_item new_int;;
let new_list_fint = _new_list_item new_fint;;
let new_list_ufint = _new_list_item new_ufint;;
let new_list_atom = _new_list_item new_atom;;
let new_list_function = _new_list_item new_function;;
let new_list_string = _new_list_item new_string;;
let new_list_list ort lref =
  (* Get the reference to VLNil. *)
  let nref = new_list ort
  in _append_list_elem ort lref nref; nref;;
let new_list_tuple ort lref =
  let nref = new_tuple ort
  in _append_list_elem ort lref nref; nref;;

(* As you can see, this part below is somewhat similar to the part above,
   and the sad thing is I can't do anything about it except merging Lists and
   Tuples. *)
let _append_tuple_elem ort lref r =
  let nval = match lookpup_val lref with
      OVTNil -> OVTuple([nref])
    | OVTuple(rs) -> OVTuple(nref::rs)
  in Hashtbl.replace (ort_of ort) lref nval;;

let _new_tuple_item nfun ort lref lit =
  let nref = nfun ort lit
  in _append_tuple_elem ort lref nref; nref;;

let new_tuple_int = _new_tuple_item new_int;;
let new_tuple_fint = _new_tuple_item new_fint;;
let new_tuple_ufint = _new_tuple_item new_ufint;;
let new_tuple_atom = _new_tuple_item new_atom;;
let new_tuple_function = _new_tuple_item new_function;;
let new_tuple_string = _new_tuple_item new_string;;
let new_tuple_list ort lref =
  (* Get the reference to VLNil. *)
  let nref = new_list ort
  (* And we are make a tuple here, remember! *)
  in _append_tuple_elem ort lref nref; nref;;
let new_tuple_tuple ort lref =
  let nref = new_tuple ort
  in _append_tuple_elem ort lref nref; nref;;

let new_name_backquote ort bqn =
  let nref = R((ticker_of ort) ())
  in Hashtbl.add (ort_of ort) nref OVNameBackquote(bqn);;

(* This actually shouldn't be called. *)
let new_value_backquote ort bqv =
  let nref = R((ticker_of ort) ())
  in Hashtbl.add (ort_of ort) nref OVValueBackquote(bqv);;

let init_ort =
  function ObjRefTable(ticker, ort, intp, fintp, ufintp, ap, sp, fp) as o ->
    new_atom o Uint64.zero; (* false *)
    new_atom o Uint64.one; (* true *)
    (* They are always important. *)
    Hashtbl.add o (R (ticker ())) OVLNil;
    Hashtbl.add o (R (ticker ())) OVTNil;;

