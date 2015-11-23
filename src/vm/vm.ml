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
let gort = make_ort ();;
let glookup_val r = lookup_val gort r;;

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

let rec string_of_value r =
  match (glookup_val r).v with
    OVInt(i) -> Big_int.string_of_big_int i
  | OVFixedInt(i) -> Int64.to_string i
  | OVUFixedInt(i) -> Uint64.to_string i
  | OVFloat(f) -> string_of_float f
  | OVAtom(a) -> Uint64.to_string a
  | OVString(s) -> s
  | OVList(rs) ->
    Printf.sprintf "[%s]"
      (String.concat " " (List.map string_of_value rs))
  | OVTuple(rs) ->
    Printf.sprintf "[@ %s]"
      (String.concat " " (List.map string_of_value rs))
  | OVFunction(st, mod_id, cl) ->
    Printf.sprintf "**fun: %s,%s,<closure set>" (tu64 st) (tu64 mod_id)
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
    in let is_todss_empty () = List.length (dss ()) = 0
    in let is_dsss_empty () = List.length dsss = 0

    in let push_calc nr =
         (((nr::((dss ()) |> tos |> ntos |> ntos))
           ::((dss ()) |> ntos))
          ::(ntos dsss))

    in let snd_todss _ = tos (ntos (dss ()))
    in let snd_tods () = tos (ntos (tos (dss ())))

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

    in let push_cur_ip cur_fun =
         {id = flags.module_id;
          ret_addr = next_ip;
          current_function = cur_fun}::ctxs

    in let inst = match line with Line(_, i) -> i

    in let make_lit = function
          VInt(i) -> new_int gort i
        | VFloat(f) -> new_float gort f
        | VString(str) -> new_string gort str
        | VUFixedInt(uf) -> new_ufint gort uf
        | VFixedInt(f) -> new_fint gort f
        | VAtom(a) -> new_atom gort a

    in match inst with
      MAKE_LIT(ArgLit(lit)) -> trace "making lit";
      ignore (make_lit lit);
      __exec ctxs dsss scpss flags next_ip

    | PUSH_LIT(ArgLit(lit)) -> trace "pushing lit";
      let nr = make_lit lit
      in __exec ctxs (push_tods nr) scpss flags next_ip

    | POP -> trace "popping";
      __exec ctxs (pop_tods ()) scpss flags next_ip

    | MAKE_FUN(ArgLit(VUFixedInt(ufi))) -> trace "making function";
      let _ = new_function gort (ufi, flags.module_id, Hashtbl.create 512)
      in __exec ctxs dsss scpss flags next_ip

    | PUSH_FUN(ArgLit(VUFixedInt(ufi))) -> trace "pushing function";
      (* I thought about it, and decided just saving the context and then jumping
         should do it. *)
      let nr = new_function gort (ufi, flags.module_id, Hashtbl.create 512)
      in __exec (push_cur_ip (glookup_val nr).v) dsss scpss
        {flags with is_tail_recursive_call = false}
        ufi

    | FINT_SUB -> trace "fint substracting";
      let ref1 = tods ()
      in let ref2 = snd_tods ()
      in let nr =
           (match ((glookup_val ref1).v,
                   (glookup_val ref2).v) with
             OVFixedInt(i), OVFixedInt(j) ->
             new_fint gort (Int64.sub i j)
           | _ -> failwith "Incompatible type to do FINT substraction.")
      in __exec ctxs (push_calc nr) scpss flags next_ip

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
        (branch glookup_val (tods ()) next_ip inst)

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
        (branch glookup_val (tods ()) next_ip inst)

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
      let st, mod_id, closure = match (tos ctxs).current_function with
          OVFunction(st, mod_id, closure) -> st, mod_id, closure
        | _ -> failwith "Not possible."
      in let is_partial, to_snd_ds =
           if Hashtbl.mem closure nid
           then false, Hashtbl.find closure nid
           else if flags.is_tail_recursive_call
           then if is_todss_empty ()
             then true, Uint64.zero (* Add this to the closure set. *)
             else false, tods ()
             (* Working on the same stack when tail recursing. *)
           else if is_todss_empty ()
           then true, Uint64.zero
           else false, tos (snd_todss ())
      in if is_partial
      then begin
        let new_closure = Hashtbl.copy closure
        in Hashtbl.iter (fun k v -> Hashtbl.replace new_closure k v) closure;
        let nr = new_function gort (st, mod_id, new_closure)
        (* We've created a new function with the new closure. Now it's time to
           return it. *)
        in let tctx = tos ctxs
        in let ret_ref = nr
        in __exec (ntos ctxs)
          (((ret_ref::(snd_todss ()))::(ntos (ntos (dss ()))))::(ntos dsss))
          scpss {flags with module_id = tctx.id;
                            current_module = Hashtbl.find modules tctx.id}
          tctx.ret_addr
      end else begin
        let v = glookup_val to_snd_ds
        in Hashtbl.replace gort.the_ort to_snd_ds
          {v with refc = Uint64.succ v.refc};
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
      end

    | PUSH_TAIL_NAME(ArgLit(VUFixedInt(nid)), ArgLit(VUFixedInt(n_mid))) ->
      trace "pushing tail name";
      let ref_ = if n_mid = Uint64.zero
        then lookup_name (scps ()) nid
        else (tvm_warning "Not tail calling a local name!";
          Hashtbl.find (Hashtbl.find modules n_mid).exs nid)
            (* If n_mod_id is not zero, i.e. we aren't tail calling a local
               function, I think the user's in trouble, but I'm not sure.
               Maybe a warning here? *)

      in (match (glookup_val ref_).v with
            OVFunction(st, _, _) as ovf ->
            __exec ctxs dsss scpss {flags with (* Modules should stay the same. *)
                                    is_tail_recursive_call = true}
              (Uint64.succ (Uint64.succ st))
          | _ -> failwith "Tail recursing a non function.")
          (* Bypass both the push-scope and push-stack instructions. *)

    | PUSH_NAME(ArgLit(VUFixedInt(nid)), ArgLit(VUFixedInt(n_mid))) ->
      trace "pushing name";
      let ref_ = if n_mid = Uint64.zero
        then lookup_name (scps ()) nid
        else Hashtbl.find (Hashtbl.find modules n_mid).exs nid

      in (match (glookup_val ref_).v with
            OVFunction(st, mod_id, _) as ovf ->
            __exec (push_cur_ip ovf)
              dsss scpss
              {flags with module_id = mod_id;
                          current_module = Hashtbl.find modules mod_id;
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
            __exec ctxs (push_tods ref_) scpss flags next_ip
          | _ ->
            __exec ctxs dsss scpss flags next_ip)

    | IMPORT_IMPLICIT(ArgLit(VString(mod_str)), ArgLit(VUFixedInt(uid))) ->
      trace "import implicit";
      let w_insts = find_module mod_str libpaths
      (* First, we find the instructions in this woven towel. *)
      in (Hashtbl.replace modules uid {insts = w_insts;
                                       exs = Hashtbl.create 512});
      (* Then, we add it to our currently opened modules hashtable. *)
      (__exec (push_cur_ip OVLNil) ([]::dsss) ([]::scpss)
         (* Finally, we jump over to the new module to do its initialization. *)
         {flags with
          import_stack = 1::flags.import_stack;
          is_init_ext_mod = true;
          module_id = uid; current_module = Hashtbl.find modules uid} Uint64.zero)

    | IMPORT_EXPLICIT(ArgLit(VString(mod_str)), ArgLit(VUFixedInt(uid))) ->
      trace "import explicit";
      (* Roughly the same as IMPLICIT_IMPORT *)
      let w_insts = find_module mod_str libpaths
      in (Hashtbl.replace modules uid {insts = w_insts;
                                       exs = Hashtbl.create 512});
      (__exec (push_cur_ip OVLNil) ([]::dsss) ([]::scpss)
         {flags with
          import_stack = 2::flags.import_stack;
          is_init_ext_mod = true;
          module_id = uid; current_module = Hashtbl.find modules uid} Uint64.zero)

    | BIND(ArgLit(VUFixedInt(uid))) -> trace "binding"; trace_u64 uid;
      let nref = gort.nref
      in let value = glookup_val !nref
      in Hashtbl.replace gort.the_ort !nref
        {value with refc = Uint64.succ value.refc};
      (* Anything that got bound to a name, its reference count increments by 1. *)
      push_name (scps ()) uid !nref;

      __exec ctxs dsss scpss flags next_ip

    | IDLE -> trace "idling";
      __exec ctxs dsss scpss flags next_ip

    | SHOW -> trace "showing";
      let r = tos (tos (dss ()))
      in print_string (string_of_value r);
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
          let original_value = glookup_val ref_
          in if original_value.refc = Uint64.one
          then Hashtbl.remove gort.the_ort ref_
          else Hashtbl.replace gort.the_ort ref_
              {original_value with refc = Uint64.pred original_value.refc})
        (scp ());
      (* This is where we do garbage collection.
         Notice that only things in ORT gets GC'ed. Things in pools aren't GC'ed.
         If this is not good, I'll GC string pool, but leave others unchanged. *)
      __exec ctxs dsss ((ntos (scps ()))::(ntos scpss)) flags next_ip

    | TERMINATE -> trace "terminating";
      if (tos flags.import_stack) <> 0
      then let tctx = (tos ctxs)
        in trace
          (Printf.sprintf "terminating: %s, ip -> %s" (Uint64.to_string tctx.id)
             (Uint64.to_string tctx.ret_addr));
        let () = Hashtbl.iter (fun k v ->
            Hashtbl.replace cur_mod.exs k v)
            (tos (List.rev (scps ())))
        in begin
          if (tos flags.import_stack) = 1
          then Hashtbl.iter (fun k v -> Hashtbl.replace (tos (tos (ntos scpss))) k v)
              (tos (List.rev (scps ())))
              (* If it is implicit import, we have to copy whatever is in the base scope
                   of current module to the top scope of the second top scope stack of the
                   whole scope stack stack. *)
          else ();
          __exec (ntos ctxs) (ntos dsss) (ntos scpss)
            {flags with
             import_stack = ntos flags.import_stack;
             is_init_ext_mod = false;
             module_id = tctx.id; current_module = Hashtbl.find modules tctx.id}
            tctx.ret_addr
        end
      else if (tos flags.import_stack) = 2
      then let tctx = (tos ctxs)
        in let () = Hashtbl.iter (fun k v ->
            Hashtbl.replace cur_mod.exs k v)
              (tos (List.rev (scps ())))
        in __exec (ntos ctxs) (ntos dsss) (ntos scpss)
          {flags with
           import_stack = ntos flags.import_stack;
           is_init_ext_mod = false;
           module_id = tctx.id; current_module = Hashtbl.find modules tctx.id}
          tctx.ret_addr
      else exit 0
    | _ -> Printf.fprintf stderr "not implemented yet\n"; exit 0

  in Hashtbl.replace modules Uint64.zero {insts = insts;
                                          exs = Hashtbl.create 512};
  __exec ctxs_ dsss_ scpss_
    {is_tail_recursive_call = false;
     is_main = true;
     is_init_ext_mod = false;
     import_stack = [0];
     module_id = Uint64.zero;
     current_module = Hashtbl.find modules Uint64.zero}
    Uint64.zero
