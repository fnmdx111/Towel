open Stdint;;

type name_t = int;;
type asm_name_t = uint64;;
type asm_name_anno_t = string;;

type module_id_t = uint64;;
type line_no_t = int;;

type closure_t = (name_t * module_id_t, value_t) Hashtbl.t
and type_hint_t = THInt | THAtom | THFixedInt | THUFixedInt | THString
                | THFloat | THList | THPhony | THLNil
                | THFunction of type_hint_t list
                | THTuple | THTNil | THType | THAlType | THAlTypeValue
and fun_t = {st: line_no_t;
             mod_id: module_id_t;
             closure: closure_t;
             is_partial: bool;
             args: (name_t, int) Hashtbl.t}
and value_t = OVInt of Big_int.big_int
            | OVAtom of uint64
            | OVFixedInt of int64
            | OVUFixedInt of uint64
            | OVString of string
            | OVFloat of float
            | OVList of value_t list ref
            | OVPhony
            | OVFunction of fun_t
            | OVTuple of value_t list ref
            | OVNil
            | OVType of type_hint_t
            | OVAlType of unit
            | OVAlTypeValue of unit * unit;;
