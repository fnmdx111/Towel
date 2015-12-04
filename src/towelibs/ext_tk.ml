open Tk;;
open T;;
open Ext;;
open Nstack;;

let top = ref None;;

let widgets:(int, Widget.toplevel Widget.widget) Hashtbl.t = Hashtbl.create 512;;

let tkfail msg = failwith (Printf.sprintf "TK failure: %s.\n" msg);;

module SimpleTk : TowelExtTemplate =
struct
  let extcall cn dss = match cn with
      1 -> top := Some(openTk ())
    | 2 -> mainLoop ()
    | 3 -> closeTk ()
    | 4 -> update ()
    | 5 -> let s = appname_get ()
      in dspush dss (OVString(s))
    | 6 -> let s = match (dspop dss) with
          OVString(x) -> x
        | _ -> tkfail "unsupported data type for appname_set"
      in appname_set s
    | _ -> tkfail "unimplemented call number"
end

let () = __ext__ := Some(module SimpleTk : TowelExtTemplate);;

