open Switches;;
open Cseg;;
open Common;;
open Stdint;;
open Exp;;
open Tasm_ast;;
open Config;;
open Scoping;;

let global_fn_id = ref "";;
let global_snippets = ref [];;

type ctx_t = {
  sw: switches; mode: int; (* 1 for push; 2 for pat *)
  is_body: bool; is_backquoted: bool;
  scp_stk: scope list; ext_scope_meta: external_scope;
  snippets: ref asm list
};;


let nil_name = {name_repr = ""; name_type = TypeDef([TDPrimitiveType(PT_Any)])};;

let __unique64 = Common.counter ();;
let uniq64 x = Printf.sprintf ":%s-%s" !global_fn_id @@ tu64 @@ __unique64 x;;

let aggregate = List.fold_left (|~~|) cnil;;

let negate_label l = l ^ "!";;

let lmap = List.map;;
let flmap = flip List.map;;

let atom_dict = Hashtbl.create 512;;
let atom_repr_tick = Common.counter ();;
Hashtbl.replace atom_dict "false" Uint64.zero;;
Hashtbl.replace atom_dict "true" @@ atom_repr_tick ();;

let fun_tick = Common.counter ();;

let (--) x y = Printf.sprintf "%s-%s" x y;;
let (^-) x y = Printf.sprintf "%d-%s" x y;;

let exp_scope:(string, name_t) Hashtbl.t = Hashtbl.create 512;;
let exp_scope_tick = Common.counter ();;
let export ctx ns =
  let _export_one n =
    Hashtbl.replace exp_scope n.name_repr @@ lookup_name ctx.scp_stk n
  in List.iter _export_one ns; cnil;;

let line x = Line([], x);;
let lline lbs x = Line(lbs, x);;
let cline lbs inst = CLine(lbs, inst);;
let put_label lbs = cline lbs None;;

type callback_arg_t = Words of word list
                    | Word of word;;
type inst_ctx_t = {pre: (callback_arg_t -> asm);
                   post: (callback_arg_t -> asm)};;
let inst_nil_ctx = {pre = (fun _ -> cnil);
                    post = (fun _ -> cnil)};;

let find_closure ctx tree =
  (* This function is trivial. It literally traverse the given subtree of the AST,
     mimics all the scoping behavior, and tries to figure out what's in the local
     scope and what's not.
     Yes, a crippled version of g_word. *)
  (* Only find_closure when you finish generating all the code. Because you have
     to know all the names bound in current scope to know what to capture.
     OR MAYBE NOT. *)
  let table = Hashtbl.create 512

  in let add_name local_scps pn =
    let ns = match pn with
        NRegular(ns) -> ns
      | NTailCall(ns) -> ns
    in if List.length ns = 1
    then try if (Uint64.compare
                   (* Cope with is_DEBUG. *)
               (lookup_name local_scps (List.hd ns)) Uint64.zero) <> 0
    (* Search in local scopes to see if the names exists, e.g.
       fun A,
         fun B,
           bind C D
           then (C)
       if we are finding closure about the outmost function,
       A is local, B is local, C is local, D is nonlocal.

       See also the comment at Ast.WSequence. *)
        then (* Do nothing. *) ()
        else (* Outside of current scope, add to the closure set. *)
          Hashtbl.replace table
            ((lookup_name (List.tl ctx.scp_stk) (List.hd ns)), Uint64.zero) 1
      with Exc.NameNotFoundError(_) ->
        Hashtbl.replace table
          ((lookup_name (List.tl ctx.scp_stk) (List.hd ns)), Uint64.zero) 1

    else Hashtbl.replace table
        (lookup_ext_name ctx.ext_scope_meta ns) 1

  in let rec __find_in locals is_body =
       function
        Ast.WName(pn) -> add_name scps pn
      | Ast.WLiteral(pv) ->
        (match pv.value_content with
          Ast.VList(wl) -> List.iter (__find_in locals false) wl
        | Ast.VTuple(wl) -> List.iter (__find_in locals false) wl
        | _ -> ()) (* No name references in other kind of literals. *)
      | Ast.WBackquote(bq) ->
        (match bq with
           Ast.BQName(n) -> add_name n
         | Ast.BQSeq(seq) -> __find_in locals false (WSequence(seq))
         | Ast.BQValue(v) -> __find_in locals false (WLiteral(v))
         | Ast.BQBackquote(b) -> __find_in locals false (WBackquote(b)))
      | Ast.WSequence(seq) ->
        (match seq with
         (* What if you do
            fun A,
              (B
               fun C,
                 bind B D
                 also E F
                 then (C B)
               B
               E)
            B should still be in the closure set. So should E. *)
           Ast.Sequence(ws) ->
           (* This part is really pain in the butt. *)
           let new_scope =
             if is_body
             then if sw_opt_seq ctx.sw
               then locals
                 (* Again, if the sequence is a body of something, and the user
                    turns opt-seq on, no new stacks nor scopes. *)
               else push_scope locals
             else push_scope locals
           in List.iter (__find_in new_scope false) ws
         | Ast.SharedSequence(ws) ->
           (* If it's a shared sequence, do the same as a opt'ed sequence. *)
           List.iter (__find_in locals false) ws)
      | Ast.WControl(ctrl) ->
        (match ctrl with
           Ast.CtrlSeqIfForm(ifs) ->
           (match ifs with
              Ast.IfGEZ(ib) | Ast.IfGZ(ib) | Ast.IfLEZ(ib)
            | Ast.IfLZ(ib) | Ast.IfEmpty(ib) | Ast.IfNonEmpty(ib)
            | Ast.IfEZ(ib) | Ast.IfNEZ(ib) | Ast.IfT(ib) | Ast.IfF(ib) ->
              (match ib with
                 Ast.IfBody(ib1, ib2) ->
                 __find_in locals true ib1;
                 __find_in locals true ib2))
         | Ast.CtrlSeqMatchForm(Ast.PatternsAndMatches(pams)) ->
           List.iter (function Ast.PatternAndMatch(ws, body) ->
               __find_in locals true body;
               List.iter (function
                     WName(_) -> ()
                   (* Names in patterns are not to be resolved, but to be
                      bound to new values. *)
                   | _ as w -> __find_in locals false w) ws) pams)
      | Ast.WFunction(f) ->
        (match f with
           Ast.Function(args, body)
         | Ast.BQFunction(args, body) ->
           push_scope locals;
           List.iter (function
                 ArgDef(pn)
               | ArgDefWithType(pn, _) ->
                 push_name locals pn Uint64.one) args;
           __find_in locals true body)
      | Ast.WBind(bs, bt) ->
        List.iter (function
              BindBody(pn, b) -> push_name locals pn Uint64.one;
              __find_in locals false b) bs;
        __find_in locals true bt
      | Ast.WImport -> ()
      | Ast.WExport -> ()
      | Ast.WIdle -> ()
      | Ast.WPhony -> ()
  in __find_in (Hashtbl.copy ctx.scp_stk) ctx.is_body tree;
  table;;

let rec g_lit ctx inst_ctx lit =
  match lit.value_content with
    Ast.VAtom(atom) ->
    let repr =
      (try Hashtbl.find atom_dict atom.atom_name
       with Not_found ->
         let r = atom_repr_tick ()
         in Hashtbl.add atom_dict atom.atom_name r; r)
    in line (PUSH_LIT(ArgLit(VAtom(r))))
  | Ast.VFixedInt(i) ->
    line (PUSH_LIT(ArgLit(VFixedInt(i))))
  | Ast.VUFixedInt(u) ->
    line (PUSH_LIT(ArgLit(VUFixedInt(u))))
  | Ast.VInt(i) ->
    line (PUSH_LIT(ArgLit(VInt(i))))
  | Ast.VFloat(f) ->
    line (PUSH_LIT(ArgLit(VFloat(f))))
  | Ast.VString(s) ->
    line (PUSH_LIT(ArgLit(VString(s))))
  | Ast.VList(wl) ->
    (inst_ctx.pre (Words(wl)))
    |~~| (line (PUSH_LNIL))
    |~~| inst_ctx.post (Words(wl))
    |~~| (let r = flmap wl (g_word ctx inst_nil_ctx) in r)
    |~~| (line END_LIST)
  | Ast.VTuple(wl) ->
    (inst_ctx.pre (Words(wl)))
    |~~| (line (PUSH_TNIL))
    |~~| inst_ctx.post (Words(wl))
    |~~| (let r = flmap wl (g_word ctx inst_nil_ctx) in r)
    |~~| (line END_TUPLE)
  | _ -> line NOT_IMPLEMENTED

and g_import ctx imp =
  let is_explicit, ss = match imp with Ast.ExplicitImport(ss) -> true, ss
                                     | Ast.ImplicitImport(ss) -> false, ss

  in let uid = ext_scope_tick ()

  in let push_into_scope ext_scope =
       let sorted_ext_scope =
         List.sort (fun x y -> Pervasives.compare (snd x) (snd y))
         @@ Hashtbl.fold (fun k v acc -> (k, v)::acc) ext_scope []
       in List.iter (fun x ->
           let k, v = x
           in push_name ctx.scp_stk {nil_name with name_repr = k}
             (name_repr_tick ())) sorted_ext_scope

  in let rec find_module mod_str = function
        path::rest ->
        let possible_mod_path = Filename.concat path (mod_str ^ ".e")
        in if BatSys.file_exists possible_mod_path
        then let ext_scope = open_export possible_mod_path

          in if is_explicit
          then push_ext_scope ctx.ext_scope_meta ext_scope uid mod_str
          else push_into_scope ext_scope

        else find_module mod_str rest
      | [] -> failwith (Printf.sprintf "Requested module `%s' not found." mod_str)

  in let () = List.iter (fun x -> find_module x Config.libpaths) ss
  in List.fold_left (|~~|) cnil
  @@ List.map (fun x ->
      line (if is_explicit
            then IMPORT_EXPLICIT(ArgLit(VString(x)), ArgLit(VUFixedInt(uid)))
            else IMPORT_IMPLICIT(ArgLit(VString(x)), ArgLit(VUFixedInt(uid))))) ss

and g_bind ctx =
  let _g_bind_body =
    function
      Ast.BindBody(pn, b) ->
      (push_name ctx.scp_stk pn @@ name_repr_tick ());
      (cnil
       |~~| (g_word {ctx with is_body = false;
                              is_backquoted = false}
               {inst_nil_ctx
                with post =
                       fun _ -> line (BIND(ArgLit(VUFixedInt(
                           lookup_name ctx.scp_stk pn))))}
               b))
      (* Cannot do compile-time stack indexing, i.e. giving names the stack index,
         because you don't know what will come out of the calculation in `b'. *)

  in function
      Ast.BindThen(bs, b)
      -> cnil
         |~~| (let r = csnl (List.map _g_bind_body bs) in r)
         |~~| (let r = g_word {ctx with is_body = true;
                                        is_backquoted = false} inst_nil_ctx b in r)

and g_name ctx =
  let get_args ns = if List.length ns = 1
    then ArgLit(VUFixedInt(lookup_name ctx.scp_stk (List.hd ns))),
         ArgLit(VUFixedInt(Uint64.zero))
    else let n_uid, es_uid = lookup_ext_name ctx.ext_scope_meta ns
      in ArgLit(VUFixedInt(n_uid)), ArgLit(VUFixedInt(es_uid))
  in function
      Ast.NRegular(ns) ->
      let arg1, arg2 = get_args ns
      in if ctx.is_backquoted
      then line (MAKE_NAME(arg1, arg2))
      else line (PUSH_NAME(arg1, arg2))
    | Ast.NTailCall(ns) ->
      let arg1, arg2 = get_args ns
      in if ctx.is_backquoted
      then line (MAKE_NAME(arg1, arg2)) (* This is a weird case. *)
      else line (PUSH_TAIL_NAME(arg1, arg2))

and g_ctrl ctx =
  function
    Ast.CtrlSeqIfForm(i) -> g_if ctx i
  | Ast.CtrlSeqMatchForm(m) -> g_match ctx m

and g_match ctx =
  let pattern_counter = Common.counter ()
  in let _UID = uinq64 ()
  in let action_label i =
       Printf.sprintf "%s-p%s" _UID @@ tu64 i
  in let match_end_label = _UID -- "end"

  in let _g_pat_and_act =
       function
         Ast.PatternAndMatch(ws, w)
         -> let label = action_label @@ pattern_counter ()
         in let new_names = ref []

         in let map_pattern =
              function
                Ast.WName(pn) as w ->
                let n = match pn with
                    Ast.NRegular(x) -> x
                  | Ast.NTailCall(x) ->
                    (ignore @@ List.map (lookup_name ctx.scp_stk) x); x
                    (* We can't invoke a name that does not exist yet. So we'll do a
                       intented name existence checking here by looking up all the
                       names. *)

                in (match n with
                      x::[] ->
                      (try if lookup_name ctx.scp_stk x = Uint64.zero
                         then (new_names := x::!new_names;
                               push_name ctx.scp_stk x @@ name_repr_tick ())
                         else ()
                       with Exc.NameNotFoundError(_) ->
                         (new_names := x::!new_names);
                         (push_name ctx.scp_stk x @@ name_repr_tick ()))
                    (* Users might bind some new names in this pattern,
                             so if I don't deal with these new names, it
                             might result in a NameNotFoundError. *)

                    | _ -> failwith "Illegal name binding");
                (* You can't bind to names in other namespaces. *)

                g_word {ctx with mode = 2; is_body = false;
                                 is_backquoted = false}
                  inst_nil_ctx w
              (* I have to log this name into current scope if it does not
                 belong to it, because in this situation, user is binding new
                 names in patterns *)
              | _ as w ->
                g_word {ctx with mode = 2; is_body = false;
                                 is_backquoted = false} inst_nil_ctx w

         in let ret = cnil
                      |~~| (let r = aggregate (lmap map_pattern ws) in r)
                      |~~| (if sw_hungry_if ctx.sw
                            then line (HMATCH(ArgLabel(negate_label label)))
                            else line (MATCH(ArgLabel(negate_label label))))
                      |~~| (let r = g_word {ctx with mode = 1; is_body = true;
                                                     is_backquoted = false}
                                inst_nil_ctx w in r)
                      |~~| (line (JUMP(ArgLabel(match_end_label))))
                      |~~| (put_label [negate_label label])
         in List.iter (pop_name ctx.scp_stk) !new_names; ret
         (* I have to remove the new names because they belong only here,
            not the incoming patterns. *)

  in function
      Ast.PatternAndMatches(ps)
      -> cnil
         |~~| (aggregate @@ lmap _g_pat_and_act ps)
         |~~| (put_label [match_end_label], [])

and g_if ctx inst =
  let _UID = uniq64 ()
  in let brancht_id = _UID
  in let branchf_id = negate_label _UID
  in let is_hungry = sw_hungry_if ctx.sw

  in let if_body_ = function
        Ast.IfBody(bt, bf) ->
        let brancht = g_word {ctx with is_body = true;
                                       is_backquoted = false} inst_nil_ctx bt
        in let branchf = g_word {ctx with is_body = true;
                                          is_backquoted = false} inst_nil_ctx bf
        in let if_end_id = Printf.sprintf "%s-end" _UID

        in (put_label [brancht_id]) |~~| brancht
           |~~| (line (JUMP(ArgLabel(if_end_id))))
           |~~| (put_label [branchf_id]) |~~| branchf
           |~~| (put_label [if_end_id])

  in let _g_body ib i =
       cnil |~~| i |~~| (if_body_ ib)

  in let parse = let lbl = ArgLabel(branchf_id)
       in function
           Ast.IfGEZ(ib) -> ib, (if is_hungry then HJLZ(lbl) else JLZ(lbl))
         | Ast.IfGZ(ib) -> ib, if is_hungry then HJLEZ(lbl) else JLEZ(lbl)
         | Ast.IfLEZ(ib) -> ib, if is_hungry then HJGZ(lbl) else JGZ(lbl)
         | Ast.IfLZ(ib) -> ib, if is_hungry then HJGEZ(lbl) else JGEZ(lbl)
         | Ast.IfEZ(ib) -> ib, if is_hungry then HJNEZ(lbl) else JNEZ(lbl)
         | Ast.IfNEZ(ib) -> ib, if is_hungry then HJEZ(lbl) else JEZ(lbl)
         | Ast.IfT(ib) -> ib, if is_hungry then HJF(lbl) else JF(lbl)
         | Ast.IfF(ib) -> ib, if is_hungry then HJT(lbl) else JT(lbl)
         | Ast.IfEmpty(ib) -> ib, if is_hungry then HJNE(lbl) else JNE(lbl)
         | Ast.IfNonEmpty(ib) -> ib, if is_hungry then HJE(lbl) else JE(lbl)

  in let body, ins = parse inst
  in _g_body body ins

and g_backquote ctx =
  let new_ctx = {ctx with is_backquoted = true}
  in function
    Ast.BQValue(pv) -> g_lit new_ctx inst_nil_ctx pv
  | Ast.BQName(n) -> g_name new_ctx inst_nil_ctx n
  | Ast.BQSeq(seq) -> g_seq new_ctx inst_nil_ctx seq
  | Ast.BQBackquote(bq) -> g_backquote ctx bq

and g_seq ctx inst_ctx seq =
  let _UID =
    if sw_opt_seq ctx.sw
    then if ctx.is_body
      then uniq64 ()
      else "na"
    else uniq64 ()

  in let seq_st_id = Printf.sprintf "%s-st" _UID
  in let seq_end_id = Printf.sprintf "%s-end" _UID
  in let seq_real_end_id = if _UID = "na" then ""
       else Printf.sprintf "%s-real-end" _UID

  in let is_shared = match seq with
        Ast.Sequence(_) -> false
      | Ast.SharedSequence(_) -> true

  in let opt_cs what =
       if sw_opt_seq ctx.sw
       then if ctx.is_body
       (* Optimize out some of the boilerplate code based on the current
          switches settings. *)
         then cnil
         else what
       else what

  in let seq_preamble =
       let _x = if not is_shared
         then (put_label [seq_st_id])
              |~~| (line PUSH_STACK)
              |~~| (line PUSH_SCOPE)
         else put_label [seq_st_id]
       in opt_cs _x

  in let seq_postamble =
       let _x = if not is_shared
         then (put_label [seq_end_id])
              |~~| (line POP_SCOPE)
              |~~| (line RET)
         else (put_label [seq_end_id])
              |~~| (line SHARED_RET)
       in (opt_cs _x) |~~| (put_label [seq_real_end_id])

  in let body, scp_stk = match seq with
        Sequence(s) -> s, if _UID = "na"
                       then ctx.scp_stk
                       else push_scope ctx.scp_stk
      | SharedSequence(s) -> s, ctx.scp_stk

  in let lead_inst = if ctx.is_backquoted
       then (line MAKE_FUN)
       else (line PUSH_FUN)
  in let seq_insts =
       cnil
       |~~| seq_preamble
       |~~| (let r = lmap (g_word {ctx with is_body = false;
                                            is_backquoted = false;
                                            scp_stk = scp_stk}
                             inst_nil_ctx) body
             in r)
       |~~| seq_postamble

  in let closure = find_closure ctx seq
  in let closure_insts =
       Hashtbl.fold (fun k v acc ->
           let nid, esid = k
           in acc |~~| line (CLOSURE(ArgLit(VUFixedInt(nid)),
                                     ArgLit(VUFixedInt(esid)))))
         cnil closure
  in let () = ctx.snippets := seq_insts::(!ctx.snippets)
  in inst_ctx.pre Word(seq)
     |~~| lead_inst
     |~~| inst_ctx.post Word(seq)

and
