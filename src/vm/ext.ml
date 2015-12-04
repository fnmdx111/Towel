open Nstack;;
open T;;

module type TowelExtTemplate =
sig
  val extcall: int -> value_t dstack_t dstack_t -> unit
end

let __ext__ = ref None;;
let __get_ext () : (module TowelExtTemplate) =
  match !__ext__ with
    Some(m) -> m
  | None -> failwith "Extension library failed to register itself.";;
