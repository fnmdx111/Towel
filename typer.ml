open Ast
open Scoping

let compare_type tm ta =
  (* test if type of ta is compatible with that of tm *)
  match tm, ta with
    PT_Any, _ -> true
  | PT_Number, PT_Int -> true
  | PT_Number, PT_FixedInt -> true
  | PT_Number, PT_Float -> true
  | PT_Int, PT_Int -> true
  | PT_FixedInt, PT_FixedInt -> true
  | PT_Float, PT_Float -> true
  | PT_String, PT_String -> true
  | PT_List, PT_List -> true
  | PT_Module, PT_Module -> true
  | _, _ -> false;;

let compare_type_def_item tdi_m tdi_a =
  let __lookup_type = function
      TDName(n) -> lookup n
    | TDPrimitiveType(p) -> p in
  compare_type (__lookup_type tdi_m) (__lookup_type tdi_a)

let check_type tm ta =
  (* test if the type definition ta is compatible with tm *)
    let rec __check ms ags =
      match ms, ags with
        [], [] -> true
      | m::ms, ag::ags ->
        if compare_type_def_item m ag
        then __check ms ags
        else false
      | _, _ -> false in
    match tm, ta with
      TypeDef(tms), TypeDef(tas) -> __check tms tas

