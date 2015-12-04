open T;;
open Nstack;;
open Ext;;
open Stdint;;

let rndfail msg = failwith (Printf.sprintf "ExtRandom> %s" msg);;

module ExtRandom:TowelExtTemplate =
struct
  let extcall cn dss = match cn with
      1 -> Random.self_init ()
    | 2 ->
      let seed = match dspop dss with
          OVFixedInt(i) -> Int64.to_int i
        | OVUFixedInt(i) -> Uint64.to_int i
        | OVInt(i) -> Big_int.int_of_big_int i
        | OVFloat(f) -> int_of_float f
        | _ -> rndfail "invalid random seed"
      in Random.init seed
    | 3 -> dspush dss (OVFloat(Random.float 1.0))
    | _ -> rndfail "unrecognized ExtRandom cnumber"
  end

let () = __ext__ := Some(module ExtRandom:TowelExtTemplate);;

