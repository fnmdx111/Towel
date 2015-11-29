open Stdint;;

type name_t = int;;
type asm_name_t = uint64;;
type asm_name_anno_t = string;;

type module_id_t = uint64;;
type line_no_t = int;;

type closure_t = (name_t, value_t) Hashtbl.t
and type_hint_t = THInt | THAtom | THFixedInt | THUFixedInt | THString
                | THFloat | THList | THPhony | THLNil
                | THFunction of type_hint_t list
                | THTuple | THTNil | THType | THAlType | THAlTypeValue
and value_t = OVInt of Big_int.big_int
            | OVAtom of uint64
            | OVFixedInt of int64
            | OVUFixedInt of uint64
            | OVString of string
            | OVFloat of float
            | OVList of value_t list
            | OVPhony
            | OVLNil
            | OVFunction of line_no_t * module_id_t * closure_t
            | OVTuple of value_t list
            | OVTNil
            | OVType of type_hint_t
            | OVAlType of unit
            | OVAlTypeValue of unit * unit;;

