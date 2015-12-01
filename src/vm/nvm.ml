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
open Common;;

let vm_name = Uint64.to_int;;
let to_pc = Uint64.to_int;;

let modules:module_table_t = Hashtbl.create 64;;

let _MAIN_MODULE_ID = Uint64.one;;
let _SELF_MODULE_ID = Uint64.zero;;
let module_id_tick = Common.counter ();;
ignore (module_id_tick ());; (* tick for _MAIN_MODULE_ID *)

let tos = List.hd;;
let ntos = List.tl;;

let ctxs_ = [{mod_id = _MAIN_MODULE_ID; ret_addr = -1;
              curfun = {st = -1;
                        mod_id = _MAIN_MODULE_ID;
                        closure = Hashtbl.create 1;
                        is_partial = false;
                        args = Hashtbl.create 1}}];;

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
      (String.concat " " (List.map string_of_value !rs))
  | OVTuple(rs) ->
    sprintf "[@ %s]"
      (String.concat " " (List.map string_of_value !rs))
  | OVFunction(f) ->
    sprintf "**%s: %d,%s,<closure set>"
      (if f.is_partial then "partial-fun" else "fun")
      f.st (Uint64.to_string f.mod_id)
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

    in let abs_mod_id mid = Hashtbl.find curmod.imports mid

    in let dss = flags.dss

    in let push_cur_ip cur_fun =
         {mod_id = curmod.id;
          ret_addr = next_ip;
          curfun = cur_fun}::ctxs

    in let return () =
      let tctx = tos ctxs
      in let ret = dspop dss
      in let tmod = Hashtbl.find modules tctx.mod_id
      in begin
        Hashtbl.iter (fun n _ ->
            Hashtbl.remove tctx.curfun.closure (n, curmod.id))
          tctx.curfun.args;
        (* Remove all the stolen argument from closure. *)
        Hashtbl.clear tctx.curfun.args;
        (* Forget about all the things I've stolen. *)
        dspurge dss; (* Wipe out current stack for GC. *)
        dspush dss ret; (* Copy return value to caller's dss. *)
        (* If we purge (and pop) the current stack then copy the return value
           there won't be a problem if the two contexts are in the same
           module. *)
        __exec (ntos ctxs) {flags with curmod = tmod} tctx.ret_addr
      end

    in let do_import rel_uid mod_str type_ =
         let w_insts = find_module mod_str libpaths
         in let abs_uid = module_id_tick ()

         in let module_ = {id = abs_uid;
                           insts = w_insts;
                           imports = Hashtbl.create 512;
                           exs = Hashtbl.create 512;
                           scps = ref []}
         in (Hashtbl.replace modules abs_uid module_);
         (Hashtbl.replace curmod.imports rel_uid abs_uid);
         (* Update the imports proxy for mapping from rel to abs. *)
         (__exec
            (push_cur_ip {st = 0; mod_id = curmod.id;
                          closure = Hashtbl.create 1;
                          is_partial = false;
                          args = Hashtbl.create 32})
            {flags with import_stack = type_::flags.import_stack;
                        is_init_ext_mod = true;
                        curmod = module_}
            0)

    (* `dsp' points to the next slot of TOS. *)
    in let tos_idx () = ((dsp (BatDynArray.last dss)),
                         (dsp dss))

    in let push_new_list type_ =
         (* 1 for list, 2 for tuple *)
         let nr = ref []
         in let nv = if type_ = 1 then OVList(nr) else OVTuple(nr)
         in if List.length flags.list_make_stack = 0
         then begin
           dspush dss nv;
           __exec ctxs {flags with list_make_stack =
                                     nr::flags.list_make_stack}
             next_ip
         end else let cur_ref = tos flags.list_make_stack
           in let () = cur_ref := nv::(!cur_ref)
           in (* No push operation in this situation. *)
           __exec ctxs {flags with list_make_stack =
                                        nr::flags.list_make_stack}
             next_ip

    in let end_list () =
         let tos = tos flags.list_make_stack
         in tos := List.rev !tos;
         __exec ctxs {flags with list_make_stack =
                                   ntos flags.list_make_stack}
           next_ip

    in let inst =
         match line with
           Line(_, i) -> i
         | CLine(_, Some(i)) ->
           tvm_warning "found a CLine, maybe a Cseg bug?"; i
         | CLine(_, None) ->
           tvm_warning "found an empty CLine, definitely a bug."; IDLE

    in trace (sprintf "stack: %s\n" (sprint_dss dss));
    if flags.is_stepping
    then (print_string "d> "; ignore (read_line ()))
    else ();
    match inst with
      PUSH_LIT(ArgLit(lit)) -> trace "pushing lit";
      let nv = match lit with
          VInt(i) -> OVInt(i)
        | VFloat(f) -> OVFloat(f)
        | VString(s) -> OVString(s)
        | VUFixedInt(uf) -> OVUFixedInt(uf)
        | VFixedInt(f) -> OVFixedInt(f)
        | VAtom(a) -> OVAtom(a)
      in (if List.length flags.list_make_stack = 0
          then dspush dss nv
          else let cur_ref = tos flags.list_make_stack
            in cur_ref := nv::(!cur_ref));
      __exec ctxs flags next_ip

    | PUSH_LNIL -> trace "pushing lnil";
      push_new_list 1

    | PUSH_TNIL -> trace "pushing tnil";
      push_new_list 2

    | END_LIST -> trace "ending list";
      end_list ()

    | END_TUPLE -> trace "ending tuple";
      end_list ()

    | CAR -> trace "pushing list head";
      let tos = dspop dss
      in (match tos with
            OVList(rls)
          | OVTuple(rls) -> dspush dss (List.hd !rls)
          | _ -> failwith "Non hd-able value type.");
      __exec ctxs flags next_ip

    | CDR -> trace "pushing list tail";
      let tos = dspop dss
      in (match tos with
            OVList(rls) -> dspush dss (OVList(ref (List.tl !rls)))
          | OVTuple(rls) -> dspush dss (OVTuple(ref (List.tl !rls)))
          (* You've got to cdr a tuple to access the elements of it. *)
          | _ -> failwith "Non tl-able value type.");
      __exec ctxs flags next_ip

    | CONS -> trace "consing list";
      let l = dspop dss
      in let elem = dspop dss
      in (match l with
            OVList(rls) -> dspush dss (OVList(ref (elem::(!rls))))
          | _ -> failwith "Cons'ing a non-list value.");
      __exec ctxs flags next_ip

    | LIST_EMPTY -> trace "pushing is_empty_list";
      let tos = dspop dss
      in let judge rls = if List.length !rls = 0
           then OVAtom(Uint64.one)
           else OVAtom(Uint64.zero)
      in (match tos with
            OVList(rls)
          | OVTuple(rls) -> dspush dss (judge rls)
          | _ -> failwith "Non is_empty-able value type.");
      __exec ctxs flags next_ip

    | POP -> trace "popping";
      (try
         ignore (dspop dss);
         __exec ctxs flags next_ip
       with PhonyEmptyStack -> failwith "popping from empty stack; phony.")

    | PUSH_FUN(ArgLit(VUFixedInt(st))) -> trace "making function";
      let nf = OVFunction({
          st = to_pc st;
          mod_id = flags.curmod.id;
          closure = Hashtbl.create 32;
          is_partial = false;
          args = Hashtbl.create 32
        })
      in begin
        dspush dss nf;
        __exec ctxs flags next_ip
      end

    | PUSH_NAME(ArgLit(VUFixedInt(_nid)), ArgLit(VUFixedInt(mid))) ->
      trace "pushing name";
      let nid = vm_name _nid
      in let v = if mid = _SELF_MODULE_ID
           then dval dss (nlookup !(curmod.scps) nid)
           else Hashtbl.find (Hashtbl.find modules
                                (Hashtbl.find curmod.imports mid)).exs nid
      in dspush dss v;
      __exec ctxs flags next_ip

    | CALL(ArgLit(VUFixedInt(st))) -> trace "pushing function";
      let nfrec = {st = to_pc st;
                   mod_id = flags.curmod.id;
                   closure = Hashtbl.create 32;
                   is_partial = false;
                   args = Hashtbl.create 32}
      in __exec (push_cur_ip nfrec)
        {flags with is_tail_recursive_call = false}
        (to_pc st)

    | INVOKE -> trace "invoking tos";
      let f = dspop dss
      in (match f with
            OVFunction(frec) ->
            __exec (push_cur_ip frec)
              {flags with is_tail_recursive_call = false} frec.st
          | _ -> failwith "invoking non-function value")

    | SUB -> trace "substracting";
      let v1 = dspop dss
      in let v2 = dspop dss
      in dspush dss (match v1, v2 with
          (* [| v2 | v1 |], when evaluating v2 v1 -, we want v2 - v1. *)
            OVFixedInt(i), OVFixedInt(j) ->
            OVFixedInt(Int64.sub j i)
          | OVUFixedInt(i), OVUFixedInt(j) ->
            OVUFixedInt(Uint64.sub j i)
          | OVFloat(i), OVFloat(j) ->
            OVFloat(j -. i)
          | OVInt(i), OVInt(j) ->
            OVInt(Big_int.sub_big_int j i)
          | _ -> failwith "Incompatible type to do substraction.");
      __exec ctxs flags next_ip

    | TO_FINT -> trace "casting to fint";
      let v = dspop dss
      in dspush dss (OVFixedInt (match v with
            OVFixedInt(i) ->
            tvm_warning "casting from fint to fint!";
            i
          | OVUFixedInt(i) ->
            Int64.of_uint64 i
          | OVFloat(i) ->
            Int64.of_float i
          | OVString(s) ->
            (* It is really not casting here, but you get the idea. *)
            Int64.of_string s
          | OVFunction(frec) ->
            (* Leave it for debugging purposes. *)
            Int64.of_int frec.st
          | _ -> failwith "casting unsupported value to fint."));
      __exec ctxs flags next_ip

    | JUMP(ArgLit(VUFixedInt(p))) -> trace "jumping";
      __exec ctxs flags (to_pc p)

    | JE(ArgLit(VUFixedInt(p))) -> trace "je";
      let j = let r = dsis_empty dss
        in if r = 1
        then (to_pc p)
        else if r = 2 (* Phony empty stack. *)
        then (to_pc p)
        else next_ip
      in __exec ctxs flags j

    | HJE(ArgLit(VUFixedInt(p))) -> trace "hje";
      (* Really cannot do much about this duplication. *)
      let j = let r = dsis_empty dss
        in if r = 1
        then (to_pc p)
        else if r = 2
        then begin
          ignore (dspop dss); (* Pop the OVPhony. *)
          (to_pc p)
        end else next_ip
      in __exec ctxs flags j

    | HJNE(ArgLit(VUFixedInt(p)))
    | JNE(ArgLit(VUFixedInt(p))) -> trace "jne";
      let j = if dsis_empty dss = 0
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
        (branch (dstop dss) next_ip inst)

    | HJT(_)
    | HJF(_)
    | HJEZ(_)
    | HJGZ(_)
    | HJLZ(_)
    | HJNEZ(_)
    | HJGEZ(_)
    | HJLEZ(_) ->
      __exec ctxs flags
        (branch (dspop dss) next_ip inst)

    | SHARED_RET -> trace "shared returning";
      let tctx = tos ctxs
      in __exec (ntos ctxs) {flags with
                             curmod = Hashtbl.find modules tctx.mod_id}
        tctx.ret_addr

    | RET -> trace "returning";
      return ()

    | PUSH_PHONY -> trace "pushing phony";
      dspush dss OVPhony;
      __exec (ntos ctxs) flags next_ip

    | CLOSURE(ArgLit(VUFixedInt(_nid)), ArgLit(VUFixedInt(mid))) ->
      trace "adding to closure";
      let nid = vm_name _nid
      in (match dstop dss with
            OVFunction(f) ->
            if mid = _SELF_MODULE_ID
            then Hashtbl.replace f.closure (nid, curmod.id)
                (dval dss (nlookup !(curmod.scps) nid))
            else Hashtbl.replace f.closure (nid, abs_mod_id mid)
                (Hashtbl.find
                   (Hashtbl.find modules (abs_mod_id mid)).exs
                   nid)
          | _ -> failwith "Add captured value to non-function.
Something is wrong with the compiler.");
      __exec ctxs flags next_ip

    | FUN_ARG(ArgLit(VUFixedInt(_nid))) -> trace "fun argumenting";
      let nid = vm_name _nid
      in let f = (tos ctxs).curfun
      in let remove_phony_if_any ds =
           let r = dis_empty ds
           in if r = 1
           then () (* It's really empty. *)
           else if r = 2
           then ignore (dpop ds)

      in let steal_arg () =
           Hashtbl.replace f.args nid 1;
           (* Make a note that we stole this argument, and are going to
              erase it from the closure set after we exit this function. *)
           if flags.is_tail_recursive_call
           then if dsis_empty dss <> 0
             then begin
               if dsis_empty dss = 2
               (* Definitely want to remove this phony, or the code after
                  this function won't work. *)
               then remove_phony_if_any (BatDynArray.last dss)
               (* The difference with non tail recursive functions is that
                  we steal arguments from different stacks. *)
               else ();
               true, OVNil
             end else false, dspop dss
           else let second_level_stack = snd_ds dss
             in if dis_empty second_level_stack <> 0
             then begin
               if dis_empty second_level_stack = 2
               then remove_phony_if_any second_level_stack
               else ();
               true, OVNil
             end else
               false, dpop second_level_stack

      in let is_partial, stolen_arg =
           if f.is_partial
           then if Hashtbl.mem f.closure (nid, curmod.id)
             then false, Hashtbl.find f.closure (nid, curmod.id)
             (* We found what we want in the closure set. Just get the
                argument from it.

                This must be checked first to ensure that partial applied
                functions work as expected. *)

             (* Otherwise, we steal argument from others. *)
             else steal_arg ()
           else steal_arg ()
      in if is_partial
      then begin
        let new_closure = Hashtbl.copy f.closure
        in Hashtbl.iter (fun name idx -> Hashtbl.replace new_closure
                            (name, curmod.id)
                            (* Fun-args are bound by default in current
                               module. *)
                            (dval dss idx))
          (tos !(curmod.scps));
        let nf = OVFunction({f with closure = new_closure;
                                    is_partial = true;
                                    args = Hashtbl.create 32})
        in dspush dss nf;
        return ()
      end else begin
        Hashtbl.replace f.closure (nid, curmod.id) stolen_arg;
        __exec ctxs flags next_ip
      end

    | REVERSE -> trace "reversing";
      let n = Uint64.to_int (match dspop dss with
            OVUFixedInt(u) -> u
          | _ -> failwith "Non-compatible type for reverse.")
      in let end_ = (BatDynArray.length (BatDynArray.last dss)) - 1
      in let st = if end_ - n + 1 >= 0 then end_ - n + 1 else 0
      in let rec _swap_them_all idx1 idx2 =
           if idx1 < idx2
           then begin
             dswap (BatDynArray.last dss) idx1 idx2;
             _swap_them_all (succ idx1) (pred idx2)
           end else ()
      in _swap_them_all st end_;
      __exec ctxs flags next_ip

    | EVAL_TAIL(ArgLit(VUFixedInt(_nid)), ArgLit(VUFixedInt(mid))) ->
      trace "tail recursive call";
      let nid = vm_name _nid
      in let v = if mid = _SELF_MODULE_ID
           then let f = (tos ctxs).curfun
             in try dval dss (nlookup !(curmod.scps) nid)
             with Exc.NameNotFoundError _ ->
               Hashtbl.find f.closure (nid, curmod.id)

           else begin
             tvm_warning "Not tail calling a module-local name!
 Maybe a mutual tail call?";
             let mod_ = Hashtbl.find modules (abs_mod_id mid)
             in Hashtbl.find mod_.exs nid
           end

      in (match v with
            OVFunction(frec) ->
            let new_ctxs = {(tos ctxs) with curfun = frec}::(ntos ctxs)
            in let tail_ip = frec.st |> succ |> succ
            (* Bypass both the push-scope and push-stack instructions. *)
            in if frec.mod_id = curmod.id
            then __exec new_ctxs
                {flags with is_tail_recursive_call = true}
                tail_ip
            else __exec new_ctxs
                {flags with is_tail_recursive_call = true;
                            curmod = Hashtbl.find modules frec.mod_id}
                tail_ip
          | _ -> failwith "Tail recursing a non-function.")

      | EVAL_AND_PUSH(ArgLit(VUFixedInt(_nid)), ArgLit(VUFixedInt(mid))) ->
        trace "evaluating name";
        let nid = vm_name _nid
        in let v = if mid = _SELF_MODULE_ID
             then let frec = (tos ctxs).curfun
               in try dval dss (nlookup !(curmod.scps) nid)
               with Exc.NameNotFoundError _ ->
                 Hashtbl.find frec.closure (nid, curmod.id)

             else let mod_ = Hashtbl.find modules (abs_mod_id mid)
               in Hashtbl.find mod_.exs nid
        in (match v with
              OVFunction(frec) ->
              __exec (push_cur_ip frec)
                {flags with curmod = Hashtbl.find modules frec.mod_id;
                            is_tail_recursive_call = false}
                frec.st

            | OVInt(_)
            | OVAtom(_)
            | OVFixedInt(_)
            | OVUFixedInt(_)
            | OVString(_)
            | OVFloat(_)
            | OVList(_)
            | OVTuple(_)
            | OVNil
            | OVType(_) ->
              dspush dss v;
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
        trace (Printf.sprintf "(%d,%d)" (fst @@ tos_idx ()) (snd @@ tos_idx ()));
        npush !(curmod.scps) (vm_name uid) (tos_idx ());
        __exec ctxs flags next_ip

      | IDLE -> trace "idling";
        __exec ctxs flags next_ip

      | SHOW -> trace "showing";
        print_string (string_of_value (dspop dss));
        __exec ctxs flags next_ip

      | PUSH_STACK -> trace "pushing stack";
        BatDynArray.add dss (dinit ());
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

      | DINT -> trace "setting step debug mode";
        __exec ctxs {flags with is_stepping = not flags.is_stepping} next_ip

      | TERMINATE -> trace "terminating";
        if tos flags.import_stack <> 0
        then let tctx = tos ctxs
          in let tmod = Hashtbl.find modules tctx.mod_id

          in let cur_base_scope = tos (List.rev (!(curmod.scps)))

          in trace (sprintf "terminating: %s, ip -> %d"
                      (Uint64.to_string tctx.mod_id) tctx.ret_addr);

          (* This implementation assumes the imported module only increase
             SP of dss by one, i.e. it pops all the stacks it created
             except for the base stack. Because all the exportable values
             are on it. *)
          let () = Hashtbl.iter (fun name vidx ->
              Hashtbl.replace curmod.exs name (dval dss vidx))
              cur_base_scope;
            (* Copy the names and values in the base scope to `exs'. *)
            (* Then purge it. *)
            dspurge dss
          in begin
            if (tos flags.import_stack) = 1
            then Hashtbl.iter (fun name value ->
                let top_of_tmod_scps = tos (!(tmod.scps))
                in let new_vidx =
                     dspush dss value; tos_idx ()
                in Hashtbl.replace top_of_tmod_scps name new_vidx)
                curmod.exs
                (* If it is implicit import, we have to copy whatever is in
                   the exs table of current module to the top scope of module
                   that imported this module. Beside that, we have to copy
                   whatever value is in the exs table of current module to
                   the top stack of dss, and redo the name-vidx mapping. *)
            else ();
            (* Pop the top scope. If nothing went wrong, it should be the
               only scope in the scope stack of the module. *)
            curmod.scps := ntos !(curmod.scps);

            __exec (ntos ctxs)
              {flags with import_stack = ntos flags.import_stack;
                          curmod = tmod}
              tctx.ret_addr
          end
        else exit 0

      | _ -> fprintf stderr "Not implemented yet.\n"; exit 0

  in Hashtbl.replace modules _MAIN_MODULE_ID
    {id = _MAIN_MODULE_ID;
     insts = insts;
     imports = Hashtbl.create 64;
     exs = Hashtbl.create 1;
     scps = ref []};

  __exec ctxs_ {is_tail_recursive_call = false;
                is_main = true;
                is_init_ext_mod = false;
                dss = dinit ();
                import_stack = [0];
                list_make_stack = [];
                is_stepping = false;
                curmod = Hashtbl.find modules _MAIN_MODULE_ID}
    0;;

