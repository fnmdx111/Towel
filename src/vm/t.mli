open Stdint;;

type ref_t = uint64;;
type name_t = uint64;;

type module_id_t = uint64;;
type line_no_t = uint64;;

type closure_t = (name_t, ref_t) Hashtbl.t;;

type type_hint_t =
    THInt of Big_int.big_int
  | THFixedInt of int64
  | THUFixedInt of uint64
  | THFloat of float
  | THAtom of uint64
  | THString of string
  | THList of ref_t list
  | THTuple of ref_t list
  | THNBQuote of name_t
  | THVBQuote of ref_t
  | THFunction of line_no_t * module_id_t * closure_t
  | THType of type_hint_t
  | THAlType of unit
  | THAlTypeValue of unit;;

type value = OVInt of Big_int.big_int
           | OVAtom of uint64
           | OVFixedInt of int64
           | OVUFixedInt of uint64
           | OVString of string
           | OVFloat of float
           | OVList of ref_t list
           | OVLNil
           | OVNameBackquote of name_t
           | OVValueBackquote of ref_t
           | OVFunction of line_no_t * module_id_t * closure_t
           | OVTuple of ref_t list
           | OVTNil
           | OVType of type_hint_t
           | OVAlType of unit
           | OVAlTypeValue of unit * unit;;

type value_t = {v: value; refc: uint64};;

