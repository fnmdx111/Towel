open Stdint;;

type ref_t = uint64;;
type name_t = uint64;;

type type_hint_t =
    THInt | THFixedInt | THUFixedInt | THFloat | THAtom | THString | THList
  | THTuple | THNBQuote | THVBQuote | THFunction | THType | THAlType
  | THAlTypeValue;;

type value = OVInt of Big_int.big_int
           | OVAtom of uint64
           | OVFixedInt of int64
           | OVUFixedInt of Uint64.uint64
           | OVString of string
           | OVFloat of float
           | OVList of ref_t list
           | OVLNil
           | OVNameBackquote of name_t
           | OVValueBackquote of ref_t
           | OVFunction of uint64
           | OVTuple of ref_t list
           | OVTNil
           | OVType of type_hint_t
           | OVAlType of unit
           | OVAlTypeValue of unit * unit;;


type value_t = {v: value; refc: uint64};;

