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
open Arithmetics;;
open Ext;;

let vm_name = Uint64.to_int;;
let vm_mod_id = Uint64.to_int;;
let to_pc = Uint64.to_int;;

let modules:module_table_t = Hashtbl.create 64;;

let opened_exts = Hashtbl.create 64;;

let name_id_tick_gen () =
  let table = Hashtbl.create 512
  in fun name ->
    try Hashtbl.find table name
    with Not_found -> Hashtbl.replace table name name; name;;

let _MAIN_MODULE_ID = 1;;
let _SELF_MODULE_ID = 0;;
let module_id_tick = let c = Common.counter ()
  in fun () -> vm_mod_id (c ());;
ignore (module_id_tick ());; (* tick for _MAIN_MODULE_ID *)

let ext_id_tick_gen () =
  let table = Hashtbl.create 512
  in let ext_id_tick =
       let c = Common.counter ()
       in fun () -> vm_mod_id (c ())
  in fun fn ->
    try Hashtbl.find table fn
    with Not_found -> let id = ext_id_tick ()
      in Hashtbl.replace table fn id; id;;

let ext_id_tick = ext_id_tick_gen ();;

let tos = List.hd;;
let ntos = List.tl;;

let ctxs_ = [{mod_id = _MAIN_MODULE_ID; ret_addr = -1;
              list_make_stack = [];
              is_tail_recursive_call = false;
              scp_count = 0;
              curfun = {st = -1;
                        mod_id = _MAIN_MODULE_ID;
                        closure = Hashtbl.create 1;
                        is_partial = false}}];;

let show_ctx c =
  fprintf stderr "{ctx: %d,%d}" c.mod_id c.ret_addr;;

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

let rec find_ext ext_str = function
    path::rest ->
    let possible_ext_path = Filename.concat path ext_str
    in if BatSys.file_exists possible_ext_path
    then possible_ext_path
    else find_ext ext_str rest
  | [] -> failwith (sprintf "Requested extension %s not found." ext_str);;

let rec string_of_value v =
  match v with
    OVInt(i) -> Big_int.string_of_big_int i
  | OVFixedInt(i) -> Int64.to_string i
  | OVUFixedInt(i) -> Uint64.to_string i
  | OVFloat(f) -> string_of_float f
  | OVAtom(a) ->
    if Uint64.compare a Uint64.one = 0
    then "true"
    else if Uint64.compare a Uint64.zero = 0
    then "false"
    else sprintf "%sa" (Uint64.to_string a)
  | OVString(s) -> s
  | OVList(rs) ->
    sprintf "[%s]"
      (String.concat " " (List.map string_of_value !rs))
  | OVTuple(rs) ->
    sprintf "[@ %s]"
      (String.concat " " (List.map string_of_value !rs))
  | OVFunction(f) ->
    sprintf "**%s: %d,%d,<closure set>"
      (if f.is_partial then "partial-fun" else "fun")
      f.st f.mod_id
  | OVTypeHint(t) ->
    "**type value"
  | OVPhony ->
    "$$"
  | _ -> "**abstract value";;

let exec should_trace should_warn insts =
  let rec __exec ctxs flags ip =
    let next_ip = succ ip
    in let line = flags.curmod.insts.(ip)

    in let trace msg =
         if should_trace
         then begin
           show_ctxs ctxs;
           fprintf stderr "**(%d,%d) -- %s\n"
             flags.curmod.id ip msg
         end else ()

    (* in let trace_u64 u = *)
    (*      if should_trace *)
    (*      then fprintf stderr "%s" (Uint64.to_string u) *)
    (*      else () *)

    in let tvm_warning msg =
         if should_warn
         then fprintf stderr "xx(%d,%d) -- %s\n"
             flags.curmod.id ip msg
         else ()

    in let curmod = flags.curmod

    in let dss = flags.dss
    in let scps = flags.scps

    in let list_make_stack = (tos ctxs).list_make_stack

    in let push_cur_ip cur_fun =
         let new_closure = Hashtbl.copy cur_fun.closure
         in Hashtbl.iter
             (fun k v -> if Hashtbl.mem cur_fun.closure k
               then ()
               else Hashtbl.replace new_closure k v)
             (* Copy all the values bound in the last context's function's
                closure in this function's closure (of course a copy) so that
                multiple level of binding can work. *)
             (* This is potentially memory-ineffective. But without this,
                quicksort (with bind) won't work, although I believe it
                should work with the presence of install, despite which only
                takes care for scopes created by functions. *)
             (tos ctxs).curfun.closure;
         {mod_id = curmod.id;
          is_tail_recursive_call = false;
          ret_addr = next_ip;
          scp_count = 0;
          list_make_stack = [];
          curfun = {cur_fun with closure = new_closure}}::ctxs

    in let __put_val x = if List.length list_make_stack = 0
         then dspush dss x
         else let cur_ref = tos list_make_stack
           in cur_ref := x::(!cur_ref)

    in let return _ nscps =
      let tctx = tos ctxs
      in let tmod = Hashtbl.find modules tctx.mod_id
      in begin
         (* Wipe out current stack for GC. *)
        if dsis_empty dss = 0
        then let r = dspop dss
          in begin
            dspurge dss;
            let __ret_lmstk = (ctxs |> ntos |> tos).list_make_stack
                (* second top ctx *)
            in if List.length __ret_lmstk = 0
            then dspush dss r
            else let cur_ref = tos __ret_lmstk
              in cur_ref := r::(!cur_ref)
              (* Copy return value to caller's dss. *)
          end else dspurge dss;
        (* If we purge (and pop) the current stack then copy the return value
           there won't be a problem if the two contexts are in the same
           module. *)
        __exec (ntos ctxs) {flags with curmod = tmod;
                                       scps = nscps} tctx.ret_addr
      end

    in let inc_scp_count () =
         {(tos ctxs) with scp_count = succ (tos ctxs).scp_count}::
         (ntos ctxs)

    in let invoke_regular fr =
         let nmod = Hashtbl.find modules fr.T.mod_id
         in __exec (push_cur_ip fr)
           {flags with curmod = nmod}
           fr.st

    (* `dsp' points to the next slot of TOS. *)
    in let tos_idx () = ((dsp (BatDynArray.last dss)),
                         (dsp dss))

    in let __push_new_ref_to_toctxs nr =
         {(tos ctxs) with list_make_stack = nr::list_make_stack}
         ::(ntos ctxs)

    in let push_new_list type_ =
         (* 1 for list, 2 for tuple *)
         let nr = ref []
         in let nv = if type_ = 1 then OVList(nr) else OVTuple(nr)
         in if List.length list_make_stack = 0
         then begin
           dspush dss nv;
           __exec (__push_new_ref_to_toctxs nr) flags next_ip
         end else let cur_ref = tos list_make_stack
           in let () = cur_ref := nv::(!cur_ref)
           in (* No push operation in this situation. *)
           __exec (__push_new_ref_to_toctxs nr) flags next_ip

    in let end_list () =
         let _tos = tos list_make_stack
         in _tos := List.rev !_tos;
         __exec ({(tos ctxs) with list_make_stack = ntos list_make_stack}
                 ::(ntos ctxs))
           flags
           next_ip

    in let resolve_name n m =
         let m = Hashtbl.find curmod.imports m
         in if m <> curmod.id
         then Hashtbl.find (Hashtbl.find modules m).exs n
             (* This module is destined to exist.
                1. You have to import the module's .e file to be
                able to compile code that actually uses values in
                this module.
                2. Therefore when you resolving names in this module,
                this module is destined to be imported already.
                3. Therefore you can't miss this name here.
                4. So no need for save values other than current module
                to closure. *)

         else try let rn, rm = nlookup scps (n, m)
             in if rm < 0
             then (* It's in closure. *)
               !(Hashtbl.find (tos ctxs).curfun.closure (rn, -rm))
             else dval dss (rn, rm)
           (* Check local scope first, because names captured in closure
              might be shadowed in current scope. *)
           with Exc.NameNotFoundError(_) -> failwith "name not found"
              | Not_found -> failwith "external name not found in tctx.curfun.closure"

    in let inst =
         match line with
           Line(_, i) -> i
         | CLine(_, Some(i)) ->
           tvm_warning "found a CLine, maybe a Cseg bug?"; i
         | CLine(_, None) ->
           tvm_warning "found an empty CLine, definitely a bug."; IDLE

    in let put_val x = __put_val x;
         __exec ctxs flags next_ip

    in trace (sprint_dss dss);
    match inst with
      PUSH_LIT(ArgLit(lit)) -> trace "pushing lit";
      let nv = match lit with
          VInt(i) -> OVInt(i)
        | VFloat(f) -> OVFloat(f)
        | VString(s) -> OVString(s)
        | VUFixedInt(uf) -> OVUFixedInt(uf)
        | VFixedInt(f) -> OVFixedInt(f)
        | VAtom(a) -> OVAtom(a)
      in put_val nv

    | PUSH_LNIL -> trace "pushing lnil";
      push_new_list 1

    | PUSH_TNIL -> trace "pushing tnil";
      push_new_list 2

    | END_LIST -> trace "ending list";
      end_list ()

    | END_TUPLE -> trace "ending tuple";
      end_list ()

    | LOAD_EXT -> trace "loading ext";
      let name =  (match dspop dss with
            OVString(s) -> s
          | _ -> failwith "incompatible type of argument to load-ext")
      in let fn = find_ext (Dynlink.adapt_filename name) libpaths
      in (* Maybe need to query the table whether this ext has been loaded. *)
      (try Dynlink.loadfile fn
       with Dynlink.Error(s) ->
       match s with
         Dynlink.Not_a_bytecode_file(a) -> failwith (sprintf "1%s\n" a)
       | Dynlink.Inconsistent_import(a) -> failwith (sprintf "2%s\n" a)
       | Dynlink.Unavailable_unit(a) -> failwith (sprintf "3%s\n" a)
       | Dynlink.Unsafe_file -> failwith (sprintf "4%s\n" "unsafe file")
       | Dynlink.Linking_error(a, b) ->
         (match b with
            Dynlink.Undefined_global(c)
            -> failwith (sprintf "5%s\n" c)
          | Dynlink.Unavailable_primitive(c)
            -> failwith (sprintf "5%s\n" c)
          | Dynlink.Uninitialized_global(c)
            -> failwith (sprintf "5%s\n" c))
       | Dynlink.Corrupted_interface(a) -> failwith (sprintf "6%s\n" a)
       | Dynlink.File_not_found(a) -> failwith (sprintf "7%s\n" a)
       | Dynlink.Cannot_open_dll(a) -> failwith (sprintf "8%s\n" a)
       | Dynlink.Inconsistent_implementation(a) -> failwith (sprintf "9%s\n" a));

      let uid = ext_id_tick fn
      in let module ThisModule = (val __get_ext () : TowelExtTemplate)
      in Hashtbl.replace opened_exts uid
        (module ThisModule : TowelExtTemplate);
      dspush dss (OVUFixedInt(Uint64.of_int uid));
      __exec ctxs flags next_ip

    | EXTCALL -> trace "loading ext";
      let ext_id = match (dspop dss) with
          OVUFixedInt(i) -> Uint64.to_int i
        | _ -> failwith "incompatible type of argument 1 to extcall"
      in let cn = match (dspop dss) with
            OVUFixedInt(i) -> Uint64.to_int i
          | _ -> failwith "incompatible type of argument 2 to extcall"
      in let module E =
           (val Hashtbl.find opened_exts ext_id : TowelExtTemplate)
      in E.extcall cn dss;
      __exec ctxs flags next_ip

    | INSTALL -> trace "installing closure";
      (* This instruction effectively notifies the scoping mechanism about
         all the captured values that are in closure so that when resolving
         a name, we don't have to explicitly check the closure. *)
      let fr = (tos ctxs).curfun
      in Hashtbl.iter
        (fun k _ -> let n, m = k
          in npush scps k (n, -m)) fr.closure;
      __exec ctxs flags next_ip

    | SWEEP -> trace "sweeping for tail recursive stacks";
      if (tos ctxs).is_tail_recursive_call
      then begin
        dspurge dss;
        BatDynArray.add dss (dinit ())
      end else ();
      __exec ctxs flags next_ip

    | PACK -> trace "packing";
      let n = match (dspop dss) with
          OVUFixedInt(i) -> Uint64.to_int i
        | OVFixedInt(i) -> let r = Int64.to_int i
          in if r < -1
          then failwith "Invalid argument to pack (arg < -1)."
          else r
        | OVInt(i) -> let r = Big_int.int_of_big_int i
          in if r < -1
          then failwith "Invalid argument to pack (arg < -1)."
          else r
        | _ -> failwith "Invalid type of argument to pack."
      in if n = -1
      then let rec __pop_until_empty acc =
             let empty = dsis_empty dss
             in if empty = 0
             then __pop_until_empty ((dspop dss)::acc)
             else if empty = 1
             then acc
             else begin let ds = BatDynArray.last dss
               in BatDynArray.delete_last ds;
               acc
             end
        in let acc = __pop_until_empty []
        in dspush dss (OVList(ref acc));
             __exec ctxs flags next_ip
      else let nl = List.fold_left (fun acc _ ->
          (dspop dss)::acc) [] (BatList.range 1 `To n)
        in dspush dss (OVList(ref nl));
        __exec ctxs flags next_ip

    | UNPACK -> trace "unpacking";
      let ns = match (dspop dss) with
          OVList(rls) -> !rls
        | OVTuple(rls) -> !rls
        | _ -> failwith "Invalid type of argument to unpack."
      in List.iter (fun x -> dspush dss x) ns; (* TODO put_val or dspush? *)
      __exec ctxs flags next_ip

    | TYPE -> trace "typing";
      let n = match (dspop dss) with
          OVInt(_) -> OVTypeHint(THInt)
        | OVAtom(_) -> OVTypeHint(THAtom)
        | OVFixedInt(_) -> OVTypeHint(THFixedInt)
        | OVUFixedInt(_) -> OVTypeHint(THUFixedInt)
        | OVString(_) -> OVTypeHint(THString)
        | OVFloat(_) -> OVTypeHint(THFloat)
        | OVList(_) -> OVTypeHint(THList)
        | OVPhony -> OVTypeHint(THPhony)
        (* don't think you can do this *)
        | OVFunction(_) -> OVTypeHint(THFunction)
        | OVTuple(_) -> OVTypeHint(THTuple)
        | OVNil -> OVTypeHint(THNil)
        | OVTypeHint(_) -> OVTypeHint(THType)
      in dspush dss n;
      __exec ctxs flags next_ip

    | DUP -> trace "duplicating";
      let v = dstop dss
      in put_val v

    | READ -> trace "reading";
      let n = Pervasives.input_line Pervasives.stdin
      in dspush dss (OVString(n));
      __exec ctxs flags next_ip

    | CAR -> trace "pushing list head";
      let tos = dspop dss
      in (match tos with
            OVList(rls) -> dspush dss (List.hd !rls)
          | OVString(s) -> dspush dss (OVString(BatString.head s 1))
          | _ -> failwith "Non hd-able value type.");
      __exec ctxs flags next_ip

    | CDR -> trace "pushing list tail";
      let tos = dspop dss
      in (match tos with
            OVList(rls) -> dspush dss (OVList(ref (List.tl !rls)))
          | OVString(s) -> dspush dss (OVString(BatString.tail s 1))
          | _ -> failwith "Non tl-able value type.");
      __exec ctxs flags next_ip

    | CONS -> trace "consing list";
      let l = dspop dss
      in let elem = dspop dss
      in (match l with
            OVList(rls) -> dspush dss (OVList(ref (elem::(!rls))))
          | OVString(s) -> let ss = match elem with
                OVString(ss) -> ss
              | _ -> failwith "Cons'ing a non-string to a string."
            in dspush dss (OVString(String.concat "" [ss; s]))
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
          | OVString(s) -> if String.length s = 0
            then dspush dss (OVAtom(Uint64.one))
            else dspush dss (OVAtom(Uint64.zero))
          | _ -> failwith "Non is_empty-able value type.");
      __exec ctxs flags next_ip

    | TUPLE_AT -> trace "getting tuple element";
      let n = match dspop dss with
          OVUFixedInt(i) -> Uint64.to_int i
        | OVFixedInt(i) -> let x = Int64.to_int i
          in if x < 0
          then failwith "Invalid index for a tuple."
          else x
        | OVInt(i) -> let x = Big_int.int_of_big_int i
          in if x < 0
          then failwith "Invalid index for a tuple."
          else x
        | _ -> failwith "Invalid index type for a tuple."
      in let tuple = match dspop dss with
            OVTuple(rls) -> !rls
          | _ -> failwith "Invalid target for tuple-at."
      in dspush dss (BatList.at tuple n);
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
        })
      in put_val nf

    | PUSH_NAME(ArgLit(VUFixedInt(_nid)), ArgLit(VUFixedInt(_mid))) ->
      trace "pushing name";
      let nid = vm_name _nid
      in let mid = vm_mod_id _mid
      in let v = resolve_name nid mid
      in put_val v

    | CALL(ArgLit(VUFixedInt(st))) -> trace "pushing function";
      let nfrec = {st = to_pc st;
                   mod_id = flags.curmod.id;
                   closure = Hashtbl.create 32;
                   is_partial = false}
      in
      __exec (push_cur_ip nfrec) flags (to_pc st)

    | INVOKE -> trace "invoking tos";
      let f = dspop dss
      in (match f with
            OVFunction(frec) -> invoke_regular frec
          | _ -> failwith "invoking non-function value")

    | ADD
    | SUB
    | MUL
    | DIV
    | MOD
    | AND
    | OR
    | XOR -> binary_arithmetics inst dss;
      __exec ctxs flags next_ip

    | TO_FINT
    | TO_UFINT
    | TO_INT
    | TO_FLOAT
    | TO_STR -> conversions inst dss;
      __exec ctxs flags next_ip

    | SHL
    | SHR
    | LSHR ->
      shift_arithmetics inst dss;
      __exec ctxs flags next_ip

    | NOT ->
      unary_arithmetics inst dss;
      __exec ctxs flags next_ip

    | EQU -> trace "testing equality";
      let v1 = dspop dss
      in let v2 = dspop dss
      in let tf = (match v1, v2 with
          (* [| v2 | v1 |], when evaluating v2 v1 -, we want v2 - v1. *)
            OVFixedInt(i), OVFixedInt(j) ->
            Int64.compare i j
          | OVUFixedInt(i), OVUFixedInt(j) ->
            Uint64.compare j i
          | OVFloat(i), OVFloat(j) ->
            tvm_warning "testing equality between float numbers.";
            Pervasives.compare j i
          | OVInt(i), OVInt(j) ->
            if Big_int.eq_big_int j i then 0 else 1
          | OVAtom(i), OVAtom(j) ->
            Uint64.compare i j
          | OVString(i), OVString(j) ->
            Pervasives.compare i j
          | OVTypeHint(i), OVTypeHint(j) ->
            Pervasives.compare i j
          | _ -> 1 (* Equality of non-equal values are 1. *))
      in dspush dss (if tf = 0
                     then OVAtom(Uint64.one)
                     else OVAtom(Uint64.zero));
        __exec ctxs flags next_ip

    | JUMP(ArgLit(VUFixedInt(p))) -> trace "jumping";
      __exec ctxs flags (to_pc p)

    | JE(ArgLit(VUFixedInt(p))) -> trace "je";
      let j = let r = dsis_empty dss
        in if r = 1
        then (to_pc p)
        else if r = 2 (* Phony empty stack. *)
        then begin
          let ds = BatDynArray.last dss
          in BatDynArray.delete_last ds;
          (to_pc p)
        end else next_ip
      in __exec ctxs flags j

    | HJE(ArgLit(VUFixedInt(p))) -> trace "hje";
      (* Really cannot do much about this duplication. *)
      let j = let r = dsis_empty dss
        in if r = 1
        then (to_pc p)
        else if r = 2
        then begin
          let ds = BatDynArray.last dss
          in BatDynArray.delete_last ds; (* Pop the OVPhony. *)
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
      return () scps

    | PUSH_PHONY -> trace "pushing phony";
      put_val OVPhony; (* push-phony is a low level instruction, if we were
                          to push a phony into string, use put_val here. *)
      __exec ctxs flags next_ip

    | CLOSURE(ArgLit(VUFixedInt(_nid))) ->
      trace "adding to closure";
      let nid = vm_name _nid
      in let mid = 0
      (* Because resolve_name will absolutify mid for us, so put 0 here. *)
      in (match dstop dss with
            OVFunction(f) ->
            let v = resolve_name nid mid
            in Hashtbl.replace f.closure (nid,
                                          Hashtbl.find curmod.imports mid)
              (ref v)
          | _ -> failwith "Adding captured value to non-function.
Something is wrong with the compiler.");
      __exec ctxs flags next_ip

    | FUN_ARG(ArgLit(VUFixedInt(_nid))) -> trace "fun argumenting";
      let nid = vm_name _nid
      in let nid = curmod.name_id_tick nid
      in let _f = (tos ctxs).curfun
      in let f = {_f with closure = Hashtbl.copy _f.closure}
      (* Because we are storing function arguments in closure, we must copy
         the function every time we execute a fun-arg, just to make sure
         recursive functions work. For example, if we are recursively calling
         our function with new arguments A = 10, B = 11; if we are not copying
         the function closure table, {ctx5}'s curfun.closure.A would be 10,
         thus overwriting ctx5's execution stack.

         ({ctx5 curfun = {closure = {A = 1; B = 2}}})
         ({ctx4 curfun = {closure = {A = 3; B = 6}}})
         ({ctx3 curfun = {closure = {A = 3; B = 1}}})
         ...

         I know copying is slow, but with this fun-arg approach, I have no
         choice. *)
      in let remove_phony_if_any ds =
           let r = dis_empty ds
           in if r = 1
           then () (* It's really empty. *)
           else if r = 2
           then BatDynArray.delete_last ds

      in let steal_arg () =
           if (tos ctxs).is_tail_recursive_call
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

      in let no_more_argument, stolen_arg =
           if f.is_partial
           then if Hashtbl.mem f.closure (nid, curmod.id)
             then false, !(Hashtbl.find f.closure (nid, curmod.id))
             (* We found what we want in the closure set. Just get the
                argument from it.

                This must be checked first to ensure that partial applied
                functions work as expected. *)

             (* Otherwise, we steal argument from others. *)
             else steal_arg ()
           else steal_arg ()
      in if no_more_argument
      then begin
        let new_closure = Hashtbl.copy f.closure
        in let nf = OVFunction({f with closure = new_closure;
                                       is_partial = true})
        in dspush dss nf; (* Actually there is no point in changing the dspush
                             here. Because you cannot create a list before the
                             last fun-arg is executed. *)
        return () (ntos scps) (* This is where I found the scope leaking.
                                 Basically with partial functions' creation. *)
      end else begin
        Hashtbl.replace f.closure (nid, curmod.id) (ref stolen_arg);
        (* Store the function argument here at f.closure. *)
        npush scps (nid, curmod.id) (nid, -curmod.id);
        (* Make the function argument visible to inner scopes so that the
           following code will work.
           fun A,
             fun B,
               A *)
        __exec ({(tos ctxs) with curfun = f}::(ntos ctxs))
          (* Modify the toctx with the newly modified version of the
             function. *)
          flags next_ip
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

    | EVAL_TAIL(ArgLit(VUFixedInt(_nid)), ArgLit(VUFixedInt(_mid))) ->
      trace "tail recursive call";
      let nid = vm_name _nid
      in let mid = vm_mod_id _mid
      in let v = resolve_name nid mid

      in let rec remove_scps _scps count =
           if count = 0
           then _scps
           else remove_scps (pop_scope _scps) (pred count)

      in (match v with
            OVFunction(frec) ->
            let new_ctxs = {(tos ctxs) with curfun = frec;
                                            scp_count = 0;
                                            is_tail_recursive_call = true;
                                            list_make_stack = []}
                           ::(ntos ctxs)
            in let tail_ip = frec.st |> succ
            (* Bypass the push-scope instruction. *)
            in if frec.mod_id = curmod.id
            then __exec new_ctxs
                {flags with scps = remove_scps scps (tos ctxs).scp_count}
                tail_ip
            else __exec new_ctxs
                {flags with scps = remove_scps scps (tos ctxs).scp_count;
                            curmod = Hashtbl.find modules frec.mod_id}
                tail_ip
          | _ -> failwith "Tail recursing a non-function.")

      | EVAL_AND_PUSH(ArgLit(VUFixedInt(_nid)), ArgLit(VUFixedInt(_mid))) ->
        trace "evaluating name";
        let nid = vm_name _nid
        in let mid = vm_mod_id _mid
        in let v = resolve_name nid mid
        in (match v with
              OVFunction(frec) -> invoke_regular frec

            | OVInt(_)
            | OVAtom(_)
            | OVFixedInt(_)
            | OVUFixedInt(_)
            | OVString(_)
            | OVFloat(_)
            | OVList(_)
            | OVTuple(_)
            | OVNil
            | OVTypeHint(_) ->
              put_val v
            | _ ->
              __exec ctxs flags next_ip)

      | IMPORT(ArgLit(VUFixedInt(uid))) ->
        trace "importing";
        let mod_str = match dspop dss with
            OVString(s) -> s
          | _ -> failwith "invalid type for a mod_str, may be a compiler bug"
        in (try let _ = Hashtbl.find curmod.imports (vm_mod_id uid)
              in __exec ctxs flags next_ip (* Do nothing since we have already
                                              imported the module before. *)
            with Not_found ->
              let w_insts = find_module mod_str libpaths
              in let abs_uid = module_id_tick ()

              in let module_ = {id = abs_uid;
                                insts = w_insts;
                                imports = Hashtbl.create 512;
                                name_id_tick = name_id_tick_gen ();
                                exs = Hashtbl.create 512}

              in (Hashtbl.replace modules abs_uid module_);
              (Hashtbl.replace curmod.imports (vm_mod_id uid) abs_uid);
              (Hashtbl.replace module_.imports _SELF_MODULE_ID abs_uid);
              (* Update the imports proxy for mapping from rel to abs. *)
              (__exec
                 (push_cur_ip {st = 0; mod_id = curmod.id;
                               closure = Hashtbl.create 1;
                               is_partial = false})
                 {flags with curmod = module_}
                 0))

      | BIND(ArgLit(VUFixedInt(uid))) -> trace "binding";
        trace (Printf.sprintf "(%d,%d)" (fst @@ tos_idx ()) (snd @@ tos_idx ()));
        npush scps ((vm_name uid), curmod.id) (tos_idx ());
        let _ = curmod.name_id_tick (vm_name uid)
        in __exec ctxs flags next_ip

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
        __exec (inc_scp_count ())
          {flags with scps = push_scope scps} next_ip

      | POP_SCOPE -> trace "popping scope";
        (* No GC whatsoever, let OCaml take care of that for me. *)
        __exec ctxs {flags with scps = ntos scps} next_ip

      | DINT -> trace "setting step debug mode";
        let rec r () = let input = read_line ()
          in (match String.sub input 0 2 with
                "ss" -> fprintf stderr "scps: %s\n" (sprint_dscope_stack scps); r ()
              | "ds" -> fprintf stderr "dss: %s\n" (sprint_dss dss); r ()
              | "vi" -> fprintf stderr "val: %s\n" (string_of_value
                                                      (dval dss
                                                         ((int_of_string (read_line ())),
                                                          (int_of_string (read_line ())))));
                r ()
              | "ln" -> (try let vidx = resolve_name (int_of_string (read_line ()))
                                 (int_of_string (read_line ()))
                           in fprintf stderr "lookup: %s" (string_of_value vidx); r ()
                         with _ -> fprintf stderr "something wrong happened"; r ())
              | "nn" -> __exec ctxs {flags with is_stepping = not flags.is_stepping} next_ip
              | _ -> fprintf stderr "something wronger happened"; r ())
            in r ()

      | EXPORT(ArgLit(VUFixedInt(_n))) -> trace "exporting";
        let n = vm_name _n
        in Hashtbl.replace curmod.exs n (dval dss (nlookup scps (n, curmod.id)));
        __exec ctxs flags next_ip

      | TERMINATE -> trace "terminating";
        if List.length ctxs <> 1 (* May become a bug? Maybe not? *)
        then (let tctx = tos ctxs
              in let tmod = Hashtbl.find modules tctx.mod_id

              in trace (sprintf "terminating: %d, ip -> %d"
                          tctx.mod_id tctx.ret_addr);
              dspurge dss;
              __exec (ntos ctxs)
                {flags with curmod = tmod;
                            scps = ntos scps}
                tctx.ret_addr)
        else exit 0

      | _ -> fprintf stderr "Not implemented yet.\n"; exit 0

  in let main = {id = _MAIN_MODULE_ID;
                 insts = insts;
                 imports = Hashtbl.create 64;
                 name_id_tick = name_id_tick_gen ();
                 exs = Hashtbl.create 1}
  in Hashtbl.replace modules _MAIN_MODULE_ID main;
  Hashtbl.replace main.imports _SELF_MODULE_ID _MAIN_MODULE_ID;

  __exec ctxs_ {dss = dinit ();
                is_stepping = false;
                scps = [];
                curmod = Hashtbl.find modules _MAIN_MODULE_ID}
    0;;

