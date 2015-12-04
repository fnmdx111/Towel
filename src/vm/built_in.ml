open Stdint;;
open T;;
open Nstack;;
open Vm_t;;

let call_built_in syscall_number dss flags =
  let sysfail msg = failwith
      (Printf.sprintf "syscall %d failure: %s" syscall_number msg)
  in match syscall_number with
    1 -> (* urandom seed *)
    Random.self_init ()
  | 2 -> (* seed *)
    let seed = match dspop dss with
        OVFixedInt(i) -> Int64.to_int i
      | OVUFixedInt(i) -> Uint64.to_int i
      | OVInt(i) -> Big_int.int_of_big_int i
      | OVFloat(f) -> int_of_float f
      | _ -> sysfail "invalid random seed"
    in Random.init seed
  | 3 -> dspush dss (OVFloat(Random.float 1.0))
  | _ -> sysfail "unrecognized syscall number";;

