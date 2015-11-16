open T;;
open Stdint;;
open Tasm_ast;;
open Ort;;
open Imp;;
open Scoping;;
open Jumps;;
open Config;;
open Vm_t;;

let modules:module_table_t = Hashtbl.create 512;;

let mod_of ref_ = Hashtbl.find modules (snd ref_);;

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
let tu64 = Uint64.to_string;;

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
  | OVAtom(a) -> Uint64.to_string a
  | OVString(s) -> s
  | OVList(rs) ->
    Printf.sprintf "[%s]"
      (String.concat " " (List.map (string_of_value cur_mod) rs))
  | OVTuple(rs) ->
    Printf.sprintf "[@ %s]"
      (String.concat " " (List.map (string_of_value cur_mod) rs))
  | OVFunction(st) ->
    Printf.sprintf "**fun: %s" (tu64 st)
  | _ -> "**abstract value";;

let exec should_trace should_warn insts =
  let rec __exec ctxs dsss scpss flags ip =

    let dss = fun () -> tos dsss
    (* You have to make them lazy here, to come around the corner cases where
       there aren't any data stacks at the moment. *)
    in let scps = fun () -> tos scpss
    in let scp = fun () -> tos (scps ())

    in let tods = fun () -> (tos (tos (dss ())))

    in let push_tods x = ((x::(tos (dss ())))::(ntos (dss ())))::(ntos dsss)
    (* I know you are shouting WTF, but... it's a functional machine. *)
    in let pop_tods _ = ((ntos (tos (dss ())))::(ntos (dss ())))::(ntos dsss)
    (* FYI, "Linux has four levels of paging". I think it's ok we have three levels
       of data stacking. *)
    in let snd_todss _ = tos (ntos (dss ()))

    in let cur_mod = flags.current_module

    in let next_ip = Uint64.succ ip
    in let line = cur_mod.insts.(Uint64.to_int ip)
    (* I don't think this coercion will make a difference. *)

    in let trace msg =
         if should_trace then (show_ctxs ctxs;
           let id_ = flags.module_id
           in Printf.fprintf stderr "**(%s,%s) -- %s\n"
             (Uint64.to_string id_) (Uint64.to_string ip) msg)
         else ()

    in let trace_u64 u =
         if should_trace
         then Printf.fprintf stderr "%s" (Uint64.to_string u)
         else ()

    in let tvm_warning msg =
         if should_warn then
           let id_ = flags.module_id
           in Printf.fprintf stderr "xx(%s,%s) -- %s\n"
             (Uint64.to_string id_) (Uint64.to_string ip) msg
         else ()

    in let push_cur_ip _ = {id = flags.module_id; ret_addr = next_ip}::ctxs

    in let inst = match line with Line(_, i) -> i

    in match inst with
      PUSH_INT(ArgLit(VInt(i))) ->
      let nr = new_int cur_mod.ort i
      in __exec ctxs (push_tods nr) scpss flags next_ip
    (* Really should generate code for these and arithmetic instructions. *)

    | PUSH_FINT(ArgLit(VFixedInt(i))) -> trace "pushing fint";
      let nr = new_fint cur_mod.ort i
      in __exec ctxs (push_tods nr) scpss flags next_ip

    | PUSH_STRING(ArgLit(VString(s))) -> trace "pushing string";
      let nr = new_string cur_mod.ort s
      in __exec ctxs (push_tods nr) scpss flags next_ip

    | MAKE_FUN(ArgLit(VUFixedInt(ufi))) -> trace "making function";
      let _ = new_function cur_mod.ort ufi
      in __exec ctxs dsss scpss flags next_ip

    | PUSH_FUN(ArgLit(VUFixedInt(ufi))) -> trace "pushing function";
      (* I thought about it, and decided just saving the context and then jumping
         should do it. *)
      __exec (push_cur_ip ()) dsss scpss
        {flags with is_tail_recursive_call = false}
        ufi

    | FINT_SUB -> trace "fint substracting";
      let (r1, rmod1) as ref1 = tods ()
      in let (r2, rmod2) as ref2 = tos (ntos (tos (dss ())))
      in (match ((lookup_val (mod_of ref1).ort ref1).v,
                 (lookup_val (mod_of ref2).ort ref2).v) with
           OVFixedInt(i), OVFixedInt(j) ->
           let nr = new_fint cur_mod.ort (Int64.sub i j)
           in __exec ctxs
             (((nr::((dss ()) |> tos |> ntos |> ntos))
               ::((dss ()) |> ntos))
              ::(ntos dsss))
             scpss flags next_ip
         | _ -> failwith "Incompatible type to do FINT substraction.")

    | JUMP(ArgLit(VUFixedInt(p))) -> trace "jumping";
      __exec ctxs dsss scpss flags p

    | JE(_)
    | JT(_)
    | JF(_)
    | JNE(_)
    | JEZ(_)
    | JGZ(_)
    | JLZ(_)
    | JNEZ(_)
    | JGEZ(_)
    | JLEZ(_) ->
      __exec ctxs dsss scpss flags
        (branch (tods ()) modules next_ip inst)

    | HJE(_)
    | HJT(_)
    | HJF(_)
    | HJNE(_)
    | HJEZ(_)
    | HJGZ(_)
    | HJLZ(_)
    | HJNEZ(_)
    | HJGEZ(_)
    | HJLEZ(_) ->
      __exec ctxs (pop_tods ()) scpss flags
        (branch (tods ()) modules next_ip inst)

    | SHARED_RET -> trace "shared returning";
      let tctx = tos ctxs
      in __exec (ntos ctxs) dsss scpss
        {flags with module_id = tctx.id;
                    current_module = Hashtbl.find modules tctx.id}
        tctx.ret_addr

    | RET -> trace "returning";
      let tctx = tos ctxs
      in let ret_ref = tods ()
      in __exec (ntos ctxs)
        (((ret_ref::(snd_todss ()))::(ntos (ntos (dss ()))))::(ntos dsss))
        scpss {flags with module_id = tctx.id;
                          current_module = Hashtbl.find modules tctx.id}
        tctx.ret_addr

    | FUN_ARG(ArgLit(VUFixedInt(nid))) -> trace "fun argumenting";
      let to_snd_ds =
        if flags.is_tail_recursive_call
        then tods () (* Working on the same stack when tail recursing. *)
        else tos (snd_todss ())
      in let r_mod = mod_of to_snd_ds
      in let value = lookup_val r_mod.ort to_snd_ds
      in Hashtbl.replace r_mod.ort.the_ort to_snd_ds
        {value with refc = Uint64.succ value.refc};
      push_name (scps ()) nid to_snd_ds;

      __exec ctxs
        (if flags.is_tail_recursive_call
         then
           ((ntos (tos (dss ())))::(ntos (dss ())))::(ntos dsss)
         else
           (((to_snd_ds::(tos (dss ()))) (* put the tos of second todss onto todss *)
             ::(ntos (snd_todss ())) (* pop the tos off from second todss *)
             ::(ntos (ntos (dss ())))) (* stick along with the rest of the ds's *)
            ::(ntos dsss))) (* stick along with the rest of dss's *)
        scpss flags next_ip

    | PUSH_TAIL_NAME(ArgLit(VUFixedInt(nid)), ArgLit(VUFixedInt(n_mid))) ->
      trace "pushing tail name";
      let r, r_mod_id as ref_ = if n_mid = Uint64.zero
        then lookup_name (scps ()) nid
        else (tvm_warning "Not tail calling a local name!";
          Hashtbl.find (Hashtbl.find modules n_mid).exs nid)
            (* If n_mod_id is not zero, i.e. we aren't tail calling a local
               function, I think the user's in trouble, but I'm not sure.
               Maybe a warning here? *)

      in let value = lookup_val (mod_of ref_).ort ref_
      in (match value.v with
            OVFunction(st) ->
            __exec ctxs dsss scpss {flags with (* Modules should stay the same. *)
                                    is_tail_recursive_call = true}
              (Uint64.succ (Uint64.succ st))
          | _ -> failwith "Tail recursing a non function.")
          (* Bypass both the push-scope and push-stack instructions. *)

    | PUSH_NAME(ArgLit(VUFixedInt(nid)), ArgLit(VUFixedInt(n_mid))) ->
      trace "pushing name";
      let r, r_mid as ref_ = if n_mid = Uint64.zero
        then lookup_name (scps ()) nid
        else Hashtbl.find (Hashtbl.find modules n_mid).exs nid

      in let value = lookup_val (mod_of ref_).ort ref_
      in (match value.v with
            OVFunction(st) ->
            __exec (push_cur_ip ())
              dsss scpss
              {flags with module_id = r_mid;
                          current_module = mod_of ref_;
                          is_tail_recursive_call = false} (trace_u64 st; st)
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
            __exec ctxs (push_tods (r, r_mid)) scpss flags next_ip
          | _ ->
            __exec ctxs dsss scpss flags next_ip)

    | IMPORT_IMPLICIT(ArgLit(VString(mod_str)), ArgLit(VUFixedInt(uid))) ->
      trace "import implicit";
      let w_insts = find_module mod_str libpaths
      (* First, we find the instructions in this woven towel. *)
      in (Hashtbl.replace modules uid {insts = w_insts; ort = make_ort uid;
                                       exs = Hashtbl.create 512});
      (* Then, we add it to our currently opened modules hashtable. *)
      (__exec (push_cur_ip ()) ([]::dsss) ([]::scpss)
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
      (__exec (push_cur_ip ()) ([]::dsss) ([]::scpss)
         {flags with
          is_import = 2;
          is_init_ext_mod = true;
          module_id = uid; current_module = Hashtbl.find modules uid} Uint64.zero)

    | BIND(ArgLit(VUFixedInt(uid))) -> trace "binding"; trace_u64 uid;
      let nref = cur_mod.ort.nref
      in let value = lookup_val cur_mod.ort !nref
      in Hashtbl.replace cur_mod.ort.the_ort !nref
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
      Hashtbl.iter (fun name ref_ ->
          let r_mod = mod_of ref_
          in let original_value = lookup_val r_mod.ort ref_
          in if original_value.refc = Uint64.one
          then Hashtbl.remove r_mod.ort.the_ort ref_
          else Hashtbl.replace r_mod.ort.the_ort ref_
              {original_value with refc = Uint64.pred original_value.refc})
        (scp ());
      (* This is where we do garbage collection.
         Notice that only things in ORT gets GC'ed. Things in pools aren't GC'ed.
         If this is not good, I'll GC string pool, but leave others unchanged. *)
      __exec ctxs dsss ((ntos (scps ()))::(ntos scpss)) flags next_ip

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
          then Hashtbl.iter (fun k v -> Hashtbl.replace (tos (tos (ntos scpss))) k v)
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
