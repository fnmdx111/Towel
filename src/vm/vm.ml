open T;;
open Stdint;;
open Tasm_ast;;
open Ort;;
open Imp;;
open Scoping;;
open Config;;

type ctx_t = {id: uint64; ret_addr: uint64};;
type module_t = {insts: line array; ort: ort_t; exs: scope};;

(* In tail recursive calls, push-tail-name jumps two more lines,
   also fun-arg fetches from current data stack rather than parent
   data stack. *)
type flags_t = {is_tail_recursive_call: bool;
                is_main: bool; is_init_ext_mod: bool;
                is_import: int; (* 1 for implicit; 2 for explicit; 0 for no *)
                module_id: uint64; current_module: module_t};;

let modules:(uint64, module_t) Hashtbl.t = Hashtbl.create 512;;

let dsss_ = [[]];;
let scpss_ = [[]];;
let ctxs_ = [];;

let show_ctx c =
  Printf.fprintf stderr "{ctx: %s,%s}" (Uint64.to_string c.id) (Uint64.to_string c.ret_addr);;

let rec show_ctxs = function
  ctx::rest -> (show_ctx ctx; Printf.fprintf stderr " "; show_ctxs rest)
| [] -> Printf.fprintf stderr "\n";;

let tos = List.hd;;
let ntos = List.tl;;

let rec find_module mod_str = function
    path::rest ->
    let possible_mod_path = Filename.concat path (mod_str ^ ".w")
    in if BatSys.file_exists possible_mod_path
    then open_woven possible_mod_path
    else find_module mod_str rest
  | [] -> failwith "Requested module not found."

let rec string_of_value cur_mod r =
  match (lookup_val (Hashtbl.find modules (snd r)).ort r).v with
    OVInt(i) -> Big_int.string_of_big_int i
  | OVFixedInt(i) -> Int64.to_string i
  | OVUFixedInt(i) -> Uint64.to_string i
  | OVFloat(f) -> string_of_float f
  | OVString(s) -> s
  | OVList(rs) ->
    Printf.sprintf "[%s]"
      (String.concat " " (List.map (string_of_value cur_mod) rs))
  | OVTuple(rs) ->
    Printf.sprintf "[@ %s]"
      (String.concat " " (List.map (string_of_value cur_mod) rs))
  | _ -> "**abstract value";;

let exec should_trace insts =
  let rec __exec ctxs dsss scpss flags ip =

    let dss = fun () -> tos dsss
    in let scps = fun () -> tos scpss
    in let scp = fun () -> tos (scps ())

    in let push_todss x = ((x::(tos (dss ())))::(ntos (dss ())))::(ntos dsss)

    in let cur_mod = flags.current_module
    in let next_ip = Uint64.succ ip
    in let line = cur_mod.insts.(Uint64.to_int ip)

    in let trace msg =
         if should_trace then (show_ctxs ctxs;
           let id_ = flags.module_id
           in Printf.fprintf stderr "**(%s,%s) -- %s\n"
             (Uint64.to_string id_) (Uint64.to_string ip) msg)
         else ()

    in let push_ip mod_id = {id = mod_id; ret_addr = next_ip}::ctxs
    in let inst = match line with Line(_, i) -> i

    in match inst with
      PUSH_INT(ArgLit(VInt(i))) ->
      let nr = new_int cur_mod.ort i
      in __exec ctxs (push_todss nr) scpss flags next_ip

    | PUSH_STRING(ArgLit(VString(s))) -> trace "pushing string";
      let nr = new_string cur_mod.ort s
      in __exec ctxs (push_todss nr) scpss flags next_ip

    | MAKE_FUN(ArgLit(VUFixedInt(ufi))) -> trace "making function";
      let _ = new_function cur_mod.ort ufi
      in __exec ctxs dsss scpss flags next_ip

    | JUMP(ArgLit(VUFixedInt(p))) -> trace "jumping";
      __exec ctxs dsss scpss flags p

    | SHARED_RET -> trace "shared returning";
      let tctx = tos ctxs
      in __exec (ntos ctxs) dsss scpss
        {flags with module_id = tctx.id;
                    current_module = Hashtbl.find modules tctx.id}
        tctx.ret_addr

    | PUSH_NAME(ArgLit(VUFixedInt(nid)), ArgLit(VUFixedInt(n_mid))) ->
      trace "pushing name"; trace (Uint64.to_string nid); trace (Uint64.to_string n_mid);

      let r, r_mid = if n_mid = Uint64.zero
        then lookup_name (scps ()) nid
        else Hashtbl.find (Hashtbl.find modules n_mid).exs nid

      in let value = lookup_val (Hashtbl.find modules r_mid).ort (r, r_mid)
      in (match value.v with
            OVFunction(st) ->
            __exec ({id = flags.module_id; ret_addr = next_ip}::ctxs)
              dsss scpss
              {flags with module_id = r_mid; current_module = Hashtbl.find modules r_mid;
                          is_tail_recursive_call = false} st
          | OVInt(_)
          | OVAtom(_)
          | OVFixedInt(_)
          | OVUFixedInt(_)
          | OVString(_)
          | OVFloat(_)
          | OVList(_)
          | OVLNil
          | OVTuple(_)
          | OVTNil
          | OVType(_) ->
            __exec ctxs (push_todss (r, r_mid)) scpss flags next_ip
          | _ ->
            __exec ctxs dsss scpss flags next_ip)

    | IMPORT_IMPLICIT(ArgLit(VString(mod_str)), ArgLit(VUFixedInt(uid))) ->
      trace "import implicit";
      let w_insts = find_module mod_str libpaths
      (* First, we find the instructions in this woven towel. *)
      in (Hashtbl.replace modules uid {insts = w_insts; ort = make_ort uid;
                                       exs = Hashtbl.create 512});
      (* Then, we add it to our currently opened modules hashtable. *)
      (__exec (push_ip flags.module_id) ([]::dsss) ([]::scpss)
         (* Finally, we jump over to the new module to do its initialization. *)
         {flags with
          is_import = 1;
          is_init_ext_mod = true;
          module_id = uid; current_module = Hashtbl.find modules uid} Uint64.zero)

    | IMPORT_EXPLICIT(ArgLit(VString(mod_str)), ArgLit(VUFixedInt(uid))) ->
      trace "import explicit";
      (* Roughly the same as IMPLICIT_IMPORT *)
      let w_insts = find_module mod_str libpaths
      in (Hashtbl.replace modules uid {insts = w_insts; ort = make_ort uid;
                                       exs = Hashtbl.create 512});
      (__exec (push_ip flags.module_id) ([]::dsss) ([]::scpss)
         {flags with
          is_import = 2;
          is_init_ext_mod = true;
          module_id = uid; current_module = Hashtbl.find modules uid} Uint64.zero)

    | BIND(ArgLit(VUFixedInt(uid))) -> trace "binding";
      let nref = cur_mod.ort.nref
      in let value = Hashtbl.find cur_mod.ort.ort !nref
      in Hashtbl.replace cur_mod.ort.ort (uid, flags.module_id)
        {value with refc = Uint64.succ value.refc};
      (* Anything that got bound to a name, its reference count increments by 1. *)
      push_name (scps ()) uid !nref;

      __exec ctxs dsss scpss flags next_ip

    | SHOW -> trace "showing";
      let r = tos (tos (dss ()))
      in print_string (string_of_value cur_mod r);
      __exec ctxs
        (((ntos (tos (dss ())))::(ntos (dss ())))::(ntos dsss)) (* WTF!!! *)
        scpss flags next_ip

    | PUSH_STACK -> trace "pushing stack";
      __exec ctxs
        (([]::(dss ()))::(ntos dsss))
        scpss flags next_ip

    | SHARE_STACK -> trace "sharing stack";
      __exec ctxs dsss scpss flags next_ip

    | PUSH_SCOPE -> trace "pushing scope";
      __exec ctxs dsss
        (((Hashtbl.create 512)::(scps ()))::(ntos scpss))
        flags next_ip

    | POP_SCOPE -> trace "popping scope";
      let cur_ort = cur_mod.ort.ort
      in Hashtbl.iter (fun name ref_ ->
          let original_value = Hashtbl.find cur_ort ref_
          in Hashtbl.replace cur_ort
            ref_ {original_value with refc = Uint64.pred original_value.refc})
         (scp ());
      (* This is where we do garbage collection.
         Notice that only things in ORT gets GC'ed. Things in pools aren't GC'ed.
         If this is not good, I'll GC string pool, but leave others unchanged. *)
      Hashtbl.iter (fun ref_ value -> if Uint64.compare value.refc Uint64.zero <= 0
                     then Hashtbl.remove cur_ort ref_
                     else ())
        cur_ort

    | TERMINATE -> trace "terminating";
      if flags.is_import <> 0
      then let tctx = (tos ctxs)
        in trace
          (Printf.sprintf "terminating: %s, ip -> %s" (Uint64.to_string tctx.id)
             (Uint64.to_string tctx.ret_addr));
        let () = Hashtbl.iter (fun k v ->
            Hashtbl.replace cur_mod.exs k v)
            (tos (List.rev (scps ())))
        in begin
          if flags.is_import = 1
          then Hashtbl.iter (fun k v ->
              Hashtbl.replace (tos (tos (ntos scpss))) k v)
              (tos (List.rev (scps ())))
              (* If it is implicit import, we have to copy whatever is in the base scope
                   of current module to the top scope of the second top scope stack of the
                   whole scope stack stack. *)
          else ();

          __exec (ntos ctxs) (ntos dsss) (ntos scpss)
            {flags with
             is_import = 0;
             is_init_ext_mod = false;
             module_id = tctx.id; current_module = Hashtbl.find modules tctx.id}
            tctx.ret_addr
        end
      else if flags.is_import = 2
      then let tctx = (tos ctxs)
        in let () = Hashtbl.iter (fun k v ->
            Hashtbl.replace cur_mod.exs k v)
              (tos (List.rev (scps ())))
        in __exec (ntos ctxs) (ntos dsss) (ntos scpss)
          {flags with
           is_import = 0;
           is_init_ext_mod = false;
           module_id = tctx.id; current_module = Hashtbl.find modules tctx.id}
          tctx.ret_addr
      else exit 0
    | _ -> Printf.fprintf stderr "not implemented yet\n"; exit 0

  in Hashtbl.replace modules Uint64.zero {insts = insts;
                                          ort = make_ort Uint64.zero;
                                          exs = Hashtbl.create 512};
  __exec ctxs_ dsss_ scpss_
    {is_tail_recursive_call = false;
     is_main = true;
     is_init_ext_mod = false;
     is_import = 0;
     module_id = Uint64.zero;
     current_module = Hashtbl.find modules Uint64.zero}
    Uint64.zero
