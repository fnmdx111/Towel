open T;;
open Common;;
open Stdint;;

(* ==========================================
     Object reference table (per module)

     As a side note, each module has a object reference table and a scope of
   exported names.
   ========================================== *)

type ort_t =
  {tick: (unit -> uint64 * uint64);
   module_id: uint64;
   the_ort: (ref_t, value_t) Hashtbl.t;
   intp: (Big_int.big_int, ref_t) Hashtbl.t;
   fintp: (int64, ref_t) Hashtbl.t;
   ufintp: (uint64, ref_t) Hashtbl.t;
   atomp: (uint64, ref_t) Hashtbl.t;
   strp: (string, ref_t) Hashtbl.t;
   funp: (uint64, ref_t) Hashtbl.t;
   nref: ref_t ref;
   tuple_stk: ref_t list ref;
   list_stk: ref_t list ref};;

let new_ort module_id =
  let _tick = Common.counter ()
  in {tick = (fun () -> (_tick (), module_id));
      the_ort = Hashtbl.create 512;
      module_id = module_id;
      intp = Hashtbl.create 512;
      fintp = Hashtbl.create 512;
      ufintp = Hashtbl.create 512;
      atomp = Hashtbl.create 512;
      strp = Hashtbl.create 512;
      funp = Hashtbl.create 512;
      nref = ref (Uint64.zero, Uint64.zero);
      tuple_stk = ref [];
      list_stk = ref []};;

let set_nref_of ort r = ort.nref := r

let lookup_val ort r =
  Hashtbl.find ort.the_ort r;;

let new_integer ort type_hint =
  try
    let r = match type_hint with
        THInt(i) -> Hashtbl.find ort.intp i
      | THFixedInt(fi) -> Hashtbl.find ort.fintp fi
      | THUFixedInt(ufi) -> Hashtbl.find ort.ufintp ufi
      | THAtom(atom) -> Hashtbl.find ort.atomp atom
      | THFunction(func) -> Hashtbl.find ort.funp func
      | _ -> failwith "Incompatible type hint."
    in set_nref_of ort r; r
  with Not_found ->
    let ref_ = ort.tick ()
    in set_nref_of ort ref_;
    let x = match type_hint with
      (* There's no polymophic names in OCaml, so you can't do, e.g.,
         let x = match type_hint with
           THInt(lit) -> lit
         | THFixedInt(lit) -> lit *)
        THInt(lit) -> Hashtbl.add ort.intp lit ref_; OVInt(lit)
      | THFixedInt(lit) -> Hashtbl.add ort.fintp lit ref_; OVFixedInt(lit)
      | THUFixedInt(lit) -> Hashtbl.add ort.ufintp lit ref_; OVUFixedInt(lit)
      | THAtom(lit) -> Hashtbl.add ort.atomp lit ref_;  OVAtom(lit)
      | THFunction(lit) -> Hashtbl.add ort.funp lit ref_; OVFunction(lit)
      | _ -> failwith "Incompatible type hint."

    in Hashtbl.add ort.the_ort ref_
      {v = x;
       refc = Uint64.zero};
    ref_;;

let new_int ort i = new_integer ort (THInt(i));;
let new_fint ort fi = new_integer ort (THFixedInt(fi));;
let new_ufint ort ufi = new_integer ort (THUFixedInt(ufi));;
let new_atom ort atom = new_integer ort (THAtom(atom));;
let new_function ort func = new_integer ort (THFunction(func));;
(* Functions are just the start instruction numbers of UInt64. *)

let new_string ort lit =
  let ref_ =
    if String.length lit < 100
    then try Hashtbl.find ort.strp lit
      with Not_found ->
        let _r = ort.tick ()
        in Hashtbl.add ort.strp lit _r; _r
        (* If the length of the string is less than 100, we will cache
           it in our string pool, and then return the ref. *)
    else ort.tick ()
    (* Otherwise return a new ref directly. *)
  in set_nref_of ort ref_;
  (* No matter whether we found that string in the pool, we are going to
     set the newestly created value reference to it. Same for others. *)
  Hashtbl.add ort.the_ort ref_ {v = OVString(lit); refc = Uint64.zero};
  ref_;;

let new_float ort f =
  let ref_ = ort.tick ()
  in set_nref_of ort ref_; Hashtbl.replace ort.the_ort ref_
    {v = OVFloat(f); refc = Uint64.zero};
  ref_;;

let new_list ort =
  let ref_ = ort.tick ()
  (* We don't cache list values. *)
  in set_nref_of ort ref_;
  Hashtbl.replace ort.the_ort ref_ {v = OVLNil; refc = Uint64.zero};
  ref_;;
(** TODO here: nested (nasty) lists must be created according to a stack
    of list creating status, e.g.:
     [p | p | p | p] -- end-list -> [p | p | p] *)

let new_tuple ort =
  let ref_ = ort.tick ()
  (* Nor for tuples. *)
  in set_nref_of ort ref_;
  Hashtbl.replace ort.the_ort ref_ {v = OVTNil; refc = Uint64.zero};
  ref_;;

let _append_list_elem ort lref r =
  let nval = match (lookup_val ort lref).v with
      OVLNil -> {v = OVList([r]); refc = Uint64.zero}
    | OVList(rs) -> {v = OVList(r::rs); refc = Uint64.zero}
    | _ -> failwith "Wrong type of list constructor."
  in Hashtbl.replace ort.the_ort lref nval;;
(* This is to say, after appending the new item, we replace what was
   there with our new list. *)

let _new_list_item nfun ort lref lit =
  (* First, you make a new thing. *)
  let nref = nfun ort lit
  (* Then you can append it to the list. *)
  in set_nref_of ort nref;
  _append_list_elem ort lref nref; nref;;

let new_list_int = _new_list_item new_int;;
let new_list_fint = _new_list_item new_fint;;
let new_list_ufint = _new_list_item new_ufint;;
let new_list_atom = _new_list_item new_atom;;
let new_list_function = _new_list_item new_function;;
let new_list_string = _new_list_item new_string;;
let new_list_float = _new_list_item new_float;;
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
  let nval = match (lookup_val ort lref).v with
      OVTNil -> {v = OVTuple([r]); refc = Uint64.zero}
    | OVTuple(rs) -> {v = OVTuple(r::rs); refc = Uint64.zero}
    | _ -> failwith "Wrong type of tuple constructor."
  in Hashtbl.replace ort.the_ort lref nval;;

let _new_tuple_item nfun ort lref lit =
  let nref = nfun ort lit
  in _append_tuple_elem ort lref nref; nref;;

let new_tuple_int = _new_tuple_item new_int;;
let new_tuple_fint = _new_tuple_item new_fint;;
let new_tuple_ufint = _new_tuple_item new_ufint;;
let new_tuple_atom = _new_tuple_item new_atom;;
let new_tuple_function = _new_tuple_item new_function;;
let new_tuple_string = _new_tuple_item new_string;;
let new_tuple_float = _new_tuple_item new_float;;
let new_tuple_list ort lref =
  (* Get the reference to VLNil. *)
  let nref = new_list ort
  (* And we are make a tuple here, remember! *)
  in set_nref_of ort nref; _append_tuple_elem ort lref nref; nref;;
let new_tuple_tuple ort lref =
  let nref = new_tuple ort
  in _append_tuple_elem ort lref nref; nref;;

let new_name_backquote ort bqn =
  let nref = ort.tick ()
  in set_nref_of ort nref;
  Hashtbl.add ort.the_ort nref {v = (OVNameBackquote(bqn));
                            refc = Uint64.zero};
  nref;;

(* This actually shouldn't be called. *)
let new_value_backquote ort bqv =
  let nref = ort.tick ()
  in Hashtbl.add ort.the_ort nref {v = (OVValueBackquote(bqv));
                               refc = Uint64.zero};;

let init_ort ort =
    ignore (new_atom ort Uint64.zero); (* false *)
    ignore (new_atom ort Uint64.one);; (* true *)

let make_ort module_id =
  let ort = new_ort module_id
  in init_ort ort; ort;;
