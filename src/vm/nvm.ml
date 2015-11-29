open Batteries;;
open T;;
open Stdint;;
open Tasm_ast;;
open Imp;;
open Dscoping;; (* Change to Sscoping for static scoping. *)
open Jumps;;
open Config;;
open Vm_t;;
open Nstack;;
open Printf;;

let vm_name = Uint64.to_int;;
let to_pc = Uint64.to_int;;

let modules:module_table_t = Hashtbl.create 512;;

let tos = List.hd;;
let ntos = List.tl;;

let ctxs_ = [];;

let show_ctx c =
  fprintf stderr "{ctx: %s,%d}" (Uint64.to_string c.mod_id) c.ret_addr;;

let rec show_ctxs = function
    ctx::rest -> (show_ctx ctx; fprintf stderr " "; show_ctxs rest)
  | [] -> fprintf stderr "\n";;

let rec find_module mod_str = function
    path::rest ->
    let possible_mod_path = Filename.concat path (mod_str ^ ".w")
    in if BatSys.file_exists possible_mod_path
    then open_woven possible_mod_path
    else find_module mod_str rest
  | [] -> failwith (sprintf "Requested module %s not found." mod_str);;

let rec string_of_value v =
  match v with
    OVInt(i) -> Big_int.string_of_big_int i
  | OVFixedInt(i) -> Int64.to_string i
  | OVUFixedInt(i) -> Uint64.to_string i
  | OVFloat(f) -> string_of_float f
  | OVAtom(a) -> Uint64.to_string a
  | OVString(s) -> s
  | OVList(rs) ->
    sprintf "[%s]"
      (String.concat " " (List.map string_of_value rs))
  | OVTuple(rs) ->
    sprintf "[@ %s]"
      (String.concat " " (List.map string_of_value rs))
  | OVFunction(st, mod_id, cl) ->
    sprintf "**fun: %d,%s,<closure set>" st (Uint64.to_string mod_id)
  | _ -> "**abstract value";;

let exec should_trace should_warn insts =
  let rec __exec ctxs flags ip =
    let next_ip = succ ip
    in let line = flags.curmod.insts.(ip)

    in let trace msg =
         if should_trace
         then begin
           show_ctxs ctxs;
           fprintf stderr "**(%s,%d) -- %s\n"
             (Uint64.to_string flags.curmod.id) ip msg
         end else ()

    (* in let trace_u64 u = *)
    (*      if should_trace *)
    (*      then fprintf stderr "%s" (Uint64.to_string u) *)
    (*      else () *)

    in let tvm_warning msg =
         if should_warn
         then fprintf stderr "xx(%s,%d) -- %s\n"
             (Uint64.to_string flags.curmod.id) ip msg
         else ()

    in let curmod = flags.curmod

    in let push_cur_ip cur_fun =
         {mod_id = curmod.id;
          ret_addr = next_ip;
          curfun = cur_fun}::ctxs

    in let return () =
      let tctx = tos ctxs
      in let ret = dspop curmod.dss
      in let tmod = Hashtbl.find modules tctx.mod_id
      in begin
        dspurge curmod.dss; (* Wipe out current stack for GC. *)
        dspush tmod.dss ret; (* Copy return value to caller's dss. *)
        (* If we purge (and pop) the current stack then copy the return value
           there won't be a problem if the two contexts are in the same
           module. *)
        __exec (ntos ctxs) {flags with curmod = tmod} tctx.ret_addr
      end

    in let do_import uid mod_str type_ =
         let w_insts = find_module mod_str libpaths
         in let module_ = {id = uid;
                           insts = w_insts;
                           exs = Hashtbl.create 512;
                           scps = ref [];
                           dss = dinit ()}
         in (Hashtbl.replace modules uid module_);
         (__exec (push_cur_ip OVLNil)
            {flags with import_stack = type_::flags.import_stack;
                        is_init_ext_mod = true;
                        curmod = module_}
            0)

    in let tos_idx () = ((dsp (BatDynArray.last curmod.dss)),
                         (dsp curmod.dss))

    in let inst =
         match line with
           Line(_, i) -> i
         | CLine(_, Some(i)) ->
           tvm_warning "found a CLine, maybe a Cseg bug?"; i
         | CLine(_, None) ->
           tvm_warning "found an empty CLine, definitely a bug."; IDLE

    in match inst with
      PUSH_LIT(ArgLit(lit)) -> trace "pushing lit";
      let nv = match lit with
          VInt(i) -> OVInt(i)
        | VFloat(f) -> OVFloat(f)
        | VString(s) -> OVString(s)
        | VUFixedInt(uf) -> OVUFixedInt(uf)
        | VFixedInt(f) -> OVFixedInt(f)
        | VAtom(a) -> OVAtom(a)
      in begin
        dspush curmod.dss nv;
        __exec ctxs flags next_ip
      end

    | POP -> trace "popping";
      (try
         ignore (dspop curmod.dss);
         __exec ctxs flags next_ip
       with PhonyEmptyStack -> failwith "popping from empty stack; phony.")

    | MAKE_FUN(ArgLit(VUFixedInt(st))) -> trace "making function";
      let nf = OVFunction(to_pc st,
                          flags.curmod.id, Hashtbl.create 512)
      in begin
        dspush curmod.dss nf;
        __exec ctxs flags next_ip
      end

    | PUSH_FUN(ArgLit(VUFixedInt(st))) -> trace "pushing function";
      let nf = OVFunction(to_pc st,
                          flags.curmod.id, Hashtbl.create 512)
      in begin
        dspush curmod.dss nf;
        __exec (push_cur_ip nf) {flags with is_tail_recursive_call = false}
          (to_pc st)
      end

    | FINT_SUB -> trace "fint substracting";
      let v1 = dspop curmod.dss
      in let v2 = dspop curmod.dss
      in dspush curmod.dss (match v1, v2 with
            OVFixedInt(i), OVFixedInt(j) ->
            OVFixedInt(Int64.sub i j)
          | _ -> failwith "Incompatible type to do FINT substraction.");
      __exec ctxs flags next_ip

    | JUMP(ArgLit(VUFixedInt(p))) -> trace "jumping";
      __exec ctxs flags (to_pc p)

    | JE(ArgLit(VUFixedInt(p))) -> trace "je";
      let j = let r = dsis_empty curmod.dss
        in if r = 1
        then (to_pc p)
        else if r = 2 (* Phony empty stack. *)
        then (to_pc p)
        else next_ip
      in __exec ctxs flags j

    | HJE(ArgLit(VUFixedInt(p))) -> trace "hje";
      (* Really cannot do much about this duplication. *)
      let j = let r = dsis_empty curmod.dss
        in if r = 1
        then (to_pc p)
        else if r = 2
        then begin
          ignore (dspop curmod.dss); (* Pop the OVPhony. *)
          (to_pc p)
        end else next_ip
      in __exec ctxs flags j

    | HJNE(ArgLit(VUFixedInt(p)))
    | JNE(ArgLit(VUFixedInt(p))) -> trace "jne";
      let j = if dsis_empty curmod.dss = 0
        then (to_pc p)
        else next_ip
      in __exec ctxs flags j

    | JT(_)
    | JF(_)
    | JEZ(_)
    | JGZ(_)
    | JLZ(_)
    | JNEZ(_)
    | JGEZ(_)
    | JLEZ(_) ->
      __exec ctxs flags
        (branch (dstop curmod.dss) next_ip inst)

    | HJT(_)
    | HJF(_)
    | HJEZ(_)
    | HJGZ(_)
    | HJLZ(_)
    | HJNEZ(_)
    | HJGEZ(_)
    | HJLEZ(_) ->
      __exec ctxs flags
        (branch (dspop curmod.dss) next_ip inst)

    | SHARED_RET -> trace "shared returning";
      let tctx = tos ctxs
      in __exec (ntos ctxs) {flags with
                             curmod = Hashtbl.find modules tctx.mod_id}
        tctx.ret_addr

    | RET -> trace "returning";
      return ()

    | PUSH_PHONY -> trace "pushing phony";
      dspush curmod.dss OVPhony;
      __exec (ntos ctxs) flags next_ip

    | FUN_ARG(ArgLit(VUFixedInt(_nid))) -> trace "fun argumenting";
      let nid = vm_name _nid
      in let st, mod_id, closure = match (tos ctxs).curfun with
          OVFunction(st, mod_id, closure) -> st, mod_id, closure
        | _ -> failwith "Not possible."

      in let remove_phony_if_any ds =
           let r = dis_empty ds
           in if r = 1
           then () (* It's really empty. *)
           else if r = 2
           then ignore (dpop ds)

      in let is_partial, stolen_arg =
           if Hashtbl.mem closure nid
           then false, Hashtbl.find closure nid
           (* We found what we want in the closure set. Just get the
              argument from it.

              This must be checked first to ensure that partial applied
              functions work as expected. *)

           (* Otherwise, we steal argument from others. *)
           else if flags.is_tail_recursive_call
           (* If we are tail recursing, we definitely steal argument on
              current level of stack. *)
           then if dsis_empty curmod.dss = 2
             then begin
               (* Must remove the phony so that it won't affect others. *)
               remove_phony_if_any (BatDynArray.last curmod.dss);
               true, OVLNil (* Just a stub. *)
             end else false, dspop curmod.dss
           else let second_level_stack = snd_ds curmod.dss
             in if dis_empty second_level_stack = 2
             then begin
               remove_phony_if_any second_level_stack;
               true, OVLNil
             end else false, dpop second_level_stack

      in if is_partial
      then begin
        let new_closure = Hashtbl.copy closure
        in Hashtbl.iter (fun name idx -> Hashtbl.replace new_closure name
                            (dval curmod.dss idx))
          (tos !(curmod.scps));
        let nf = OVFunction(st, mod_id, new_closure)
        in dspush curmod.dss nf;
        return ()
      end else begin
        dspush curmod.dss stolen_arg;
        npush (!(curmod.scps)) nid (tos_idx ());
        __exec ctxs flags next_ip
      end

    | REVERSE(ArgLit(VUFixedInt(_n))) -> trace "reversing";
      let n = Uint64.to_int _n
      in let end_ = (BatDynArray.length (BatDynArray.last curmod.dss)) - 1
      in let st = if end_ - n + 1 >= 0 then end_ - n + 1 else 0
      in let rec _swap_them_all idx1 idx2 =
           dswap (BatDynArray.last curmod.dss) idx1 idx2;
           if idx1 < idx2
           then _swap_them_all (succ idx1) (pred idx2)
           else ()
      in _swap_them_all st end_;
      __exec ctxs flags next_ip

    | EVAL_TAIL(ArgLit(VUFixedInt(_nid)), ArgLit(VUFixedInt(mid))) ->
      trace "tail recursive call";
      let nid = vm_name _nid
      in let v = if mid = Uint64.zero
        then dval curmod.dss (nlookup !(curmod.scps) nid)
        else begin
          tvm_warning "Not tail calling a module-local name!
 Maybe a mutual tail call?";
          let mod_ = Hashtbl.find modules mid
          in dval mod_.dss (Hashtbl.find mod_.exs nid)
        end

      in (match v with
            OVFunction(st, mod_id, _) as f ->
            let new_ctxs = {(tos ctxs) with curfun = f}::(ntos ctxs)
            in let tail_ip = st |> succ |> succ
            (* Bypass both the push-scope and push-stack instructions. *)
            in if mod_id = curmod.id
            then __exec new_ctxs
                {flags with is_tail_recursive_call = true}
                tail_ip
            else __exec new_ctxs
                {flags with is_tail_recursive_call = true;
                            curmod = Hashtbl.find modules mod_id}
                tail_ip
          | _ -> failwith "Tail recursing a non-function.")

      | EVAL_AND_PUSH(ArgLit(VUFixedInt(_nid)), ArgLit(VUFixedInt(mid))) ->
        trace "evaluating name";
        let nid = vm_name _nid
        in let v = if mid = Uint64.zero
          then dval curmod.dss (nlookup (!(curmod.scps)) nid)
          else let mod_ = Hashtbl.find modules mid
            in dval mod_.dss (Hashtbl.find mod_.exs nid)
        in (match v with
              OVFunction(st, mod_id, _) as f ->
              __exec (push_cur_ip f)
                {flags with curmod = Hashtbl.find modules mod_id;
                            is_tail_recursive_call = false}
                st

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
              dspush curmod.dss v;
              __exec ctxs flags next_ip
            | _ ->
              __exec ctxs flags next_ip)

      | IMPORT_IMPLICIT(ArgLit(VString(mod_str)), ArgLit(VUFixedInt(uid))) ->
        trace "implicit importing";
        do_import uid mod_str 1

      | IMPORT_EXPLICIT(ArgLit(VString(mod_str)), ArgLit(VUFixedInt(uid))) ->
        trace "import explicit";
        do_import uid mod_str 2

      | BIND(ArgLit(VUFixedInt(uid))) -> trace "binding";
        npush !(curmod.scps) (vm_name uid) (tos_idx ());
        __exec ctxs flags next_ip

      | IDLE -> trace "idling";
        __exec ctxs flags next_ip

      | SHOW -> trace "showing";
        print_string (string_of_value (dstop curmod.dss));
        __exec ctxs flags next_ip

      | PUSH_STACK -> trace "pushing stack";
        BatDynArray.add curmod.dss (dinit ());
        __exec ctxs flags next_ip

      | SHARE_STACK -> trace "sharing stack";
        __exec ctxs flags next_ip

      | PUSH_SCOPE -> trace "pushing scope";
        curmod.scps := (Hashtbl.create 512)::(!(curmod.scps));
        __exec ctxs flags next_ip

      | POP_SCOPE -> trace "popping scope";
        curmod.scps := ntos (!(curmod.scps));
        (* No GC whatsoever, let OCaml take care of that for me. *)
        __exec ctxs flags next_ip

      | TERMINATE -> trace "terminating";
        if tos flags.import_stack <> 0
        then let tctx = tos ctxs
          in let tmod = Hashtbl.find modules tctx.mod_id

          in let cur_base_scope = tos (List.rev (!(curmod.scps)))

          in trace (sprintf "terminating: %s, ip -> %d"
                      (Uint64.to_string tctx.mod_id) tctx.ret_addr);
          let () = Hashtbl.iter (fun k v ->
              Hashtbl.replace curmod.exs k v)
              cur_base_scope
              (* Copy the names in the base scope to `exs' scope. *)
          in begin
            if (tos flags.import_stack) = 1
            then Hashtbl.iter (fun k v ->
                let top_of_tmod_scps = tos (!(tmod.scps))
                in Hashtbl.replace top_of_tmod_scps k v)
                cur_base_scope
                (* If it is implicit import, we have to copy whatever is in
                   the base scope of current module to the top scope of the
                   second top scope stack of the whole scope stack stack. *)
            else ();
            __exec (ntos ctxs)
              {flags with import_stack = ntos flags.import_stack;
                          curmod = tmod}
              tctx.ret_addr
          end
        else exit 0

      | _ -> fprintf stderr "Not implemented yet.\n"; exit 0

  in Hashtbl.replace modules Uint64.zero {id = Uint64.zero;
                                          insts = insts;
                                          exs = Hashtbl.create 1;
                                          scps = ref [];
                                          dss = dinit ()};
  __exec ctxs_ {is_tail_recursive_call = false;
                is_main = true;
                is_init_ext_mod = false;
                import_stack = [0];
                list_make_stack = [];
                tuple_make_stack = [];
                curmod = Hashtbl.find modules Uint64.zero}
    0;;

