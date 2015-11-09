open Ast
open Scoping
open Batteries
open Switches
open Cseg
open Common
open Stdint

let global_fn_id = ref "";;

type word_builder_mode =
    MakeOnly
  | PushMake
  | Pattern;;

type is_body_t =
    IsNotBody
  | IsBody;;

type is_shared_sequence =
    IsSharedSeq
  | IsNotSharedSeq;;

type ctx_t = {sw: switches; mode: word_builder_mode;
              scp_stk: scope list};;

type inst_ctx_t = {pre: (string -> code_segment);
                   post: (string -> code_segment)};;
(* `pre' and `post' are hooks that accepts a UID string as their argument,
   and generates appropriate code segment after the leading instruction,
   e.g. `make-list', `push-fun', etc. *)

let inst_nil_ctx = {pre = (fun _ -> cnil); post = (fun _ -> cnil)};;
(* Most of the time, you don't want to insert anything more. *)

let __unique64 = Common.counter ();;
let uniq64 x = Printf.sprintf ":%s-%s"
    !global_fn_id
  @@ tu64 @@ __unique64 x;;
(* This function generates a unique label with respect to the id of the
   source file, i.e. `global_fn_id'. The `global_fn_id' is the first ten
   character of the hex alphanumeric representation of the SHA1 digest of the
   file content. I learned this idea from git. And I think ten characters
   should suffice the requirement for uniqueness. *)

let negate_label l = l ^ "!";;

let flap = flip List.map;;

let atom_dict = Hashtbl.create 512;;
let atom_repr_tick = Common.counter ();;
(* Atoms are just names in another universe where mappings are different. *)

ignore @@ atom_repr_tick ();; (* tick tock for false - 0 *)
ignore @@ atom_repr_tick ();; (* tick tock for true - 1 *)

let name_repr_tick = Common.counter ();;
(* 2**63 names should be enough. I can switch to Stdint.Uint128 if it's
   necessary. *)

let fun_tick = Common.counter ();;

let (--) x y = Printf.sprintf "%s-%s" x y;;
let (^-) x y = Printf.sprintf "%d-%s" x y;;

(* All the g_* functions should return a code_segment value. *)
let rec g_lit ctx inst_ctx lit =
  let inst t =
    (match ctx.mode with
       MakeOnly -> "make"
     | PushMake -> "push"
     | Pattern -> "patpush") -- t

  in match lit.value_content with

    VAtom(atom) ->
    let repr =
      (try Hashtbl.find atom_dict atom.atom_name
       with Not_found ->
         let r = atom_repr_tick ()
         in Hashtbl.add atom_dict atom.atom_name r; r)
    in inst_ctx.pre "0"
       |~~| (cone1 (inst "atom") @@ tu64 repr)
       |~~| inst_ctx.post "0"

  | VFixedInt(i) ->
    inst_ctx.pre "0"
    |~~| (cone1 (inst "fint") @@ Int64.to_string i)
    |~~| inst_ctx.post "0"

  | VUFixedInt(u) ->
    inst_ctx.pre "0"
    |~~| (cone1 (inst "ufint") @@ tu64 u)
    |~~| inst_ctx.post "0"

  | VInt(i) ->
    inst_ctx.pre "0"
    |~~| (cone1 (inst "int")
          @@ Printf.sprintf "%sl"
          @@ Big_int.string_of_big_int i)
    |~~| inst_ctx.post "0"

  | VFloat(f) ->
    inst_ctx.pre "0"
    |~~| (cone1 (inst "float") @@ string_of_float f)
    |~~| inst_ctx.post "0"

  | VList(wl)
    -> cnil (* ascii art!! *)
       |~~| (inst_ctx.pre
             @@ string_of_int
             @@ (List.length wl) + 1)
       |~~| (cone0 @@ inst "list")
       |~~| (inst_ctx.post
             @@ string_of_int
             @@ (List.length wl) + 1)
       |~~| let r = csnl (flap wl @@ g_word
                            {ctx with mode = PushMake}
                            IsNotBody
                            inst_nil_ctx) in r
       |~~| (cone0 "end-list")

  | VString(s) ->
    inst_ctx.pre "0"
    |~~| (cone1 (inst "string") @@ Printf.sprintf "'%s'" s)
    |~~| inst_ctx.post "0"

  | VTuple(wl)
    -> cnil
       |~~| (inst_ctx.pre
             @@ string_of_int
             @@ (List.length wl) + 1)
       |~~| (cone0 @@ inst "tuple")
       |~~| (inst_ctx.post
             @@ string_of_int
             @@ (List.length wl) + 1)
       |~~| let r = csnl (flap wl
                          @@ g_word {ctx with mode = PushMake}
                            IsNotBody
                            inst_nil_ctx) in r
       |~~| cone0 "end-tuple"

  | VAlTypeLiteral(_) ->
    inst_ctx.pre "0"
    |~~| cone0 "not-implemented"
    |~~| inst_ctx.post "0"

and g_name ctx pn =
  let to_name_id_string ns =
    String.concat " "
    @@ List.map (fun x -> tu64 x)
    @@ List.map (lookup_name ctx.scp_stk) ns
    (* It's a series of name IDs combined by spaces and reminds me that
       my ccg.py needs some modification. *)

  in let prefix = match ctx.mode with
        PushMake -> ""
      | Pattern -> "pat"
      | MakeOnly -> "" (* What's the point in this? *)

  in let inst t = prefix ^ t -- "name"

  in let ns, t = match pn with
        NRegular(n) -> n, "push"
      | NTailCall(n) -> n, "push-tail"

  in cone1 (inst t) (to_name_id_string ns)

and g_backquote ctx =
  let bq_inst = match ctx.mode with
      MakeOnly -> "make-backquote"
    | PushMake -> "backquote"
    | Pattern -> "patbackquote"
  in function
      BQValue(pv) ->
      g_lit ctx {inst_nil_ctx with
                 post = fun rj (* reljump number deserves a r-inst *)
                   -> (cone0 bq_inst) |~~| (cone1 "rjump" rj)} pv

    | BQName(n)
      -> cnil
         |~~| cone0 "backquote-name"
         |~~| g_name ctx n

    | BQSeq(seq) ->
      g_seq ctx IsNotBody
        {inst_nil_ctx with
         post = fun uid
           -> (cone0 bq_inst)
              |~~| (cone1 "jump"
                      (uid -- "real-end"))} seq

    | BQBackquote(b) ->
      g_backquote ctx b

and g_seq ctx is_body_ inst_ctx seq =
  let _UID =
    if sw_opt_seq ctx.sw
    then match is_body_ with
        IsNotBody -> uniq64 ()
      | IsBody -> "na"
      (* do not assign a UID to a seq that is to be eliminated *)
    else uniq64 ()

  in let seq_st_id = Printf.sprintf "%s-st" _UID
  in let seq_end_id = Printf.sprintf "%s-end" _UID
  in let seq_real_end_id = if _UID = "na" then ""
         (* Eliminated sequences (i.e. those with "na" as their Ids), have
            no real end id, Because there is no cleanup actions for them. *)
           else Printf.sprintf "%s-real-end" _UID

  in let seq_type = match seq with
        Sequence(_) -> 1
      | SharedSequence(_) -> 0

  in let opt_cs what =
       if sw_opt_seq ctx.sw
       then match is_body_ with
           IsNotBody -> what
         | IsBody -> cnil
       else what

  in let seq_preamble =
       let _x = if seq_type = 1 then
           (* If this is a Sequence sequence, do all the initializations
              with a start label; *)
           LabeledCodeSegment([seq_st_id],
                              [cone0 "push-stack";
                               cone0 "push-scope"])
             (* otherwise, do none of the initializations with a start
                label.
                This start label is important for recurive invocations to
                this shared sequence.
             *)
         else LabeledCodeSegment([seq_st_id], [])
       in opt_cs _x

  in let seq_postamble =
       let _x = if seq_type = 1 then
         (* For regular sequences, the clean-ups are the same with functions,
            popping the scope and return the return value. *)
           LabeledCodeSegment([seq_end_id], [cone0 "pop-scope";
                                             cone0 "ret"])
             (* For shared sequences, we do none of the clean-ups, and
                the end label is not important at all. But for the sake of
                symmetricness, we put it here alright. *)
         else LabeledCodeSegment([seq_end_id], [cone0 "shared-ret"])
       in (opt_cs _x) |~~| LabeledCodeSegment([seq_real_end_id], [])

  in let body, scp_stk = match seq with
        Sequence(s) -> s, if _UID = "na"
                       then ctx.scp_stk (* Leave the scope stack be *)
                       else push_scope ctx.scp_stk
      | SharedSequence(s) -> s, ctx.scp_stk

  in let lead_inst = match ctx.mode with
        MakeOnly -> cone1 "make-fun" seq_st_id
      | PushMake -> cone1 "push-fun" seq_st_id
      | Pattern -> cone1 "patpush-fun" seq_st_id

  in cnil
     |~~| inst_ctx.pre _UID
     |~~| (opt_cs lead_inst)
     |~~| inst_ctx.post _UID
     |~~| seq_preamble
     |~~| let r = csnl (flap body
                        @@ g_word {ctx with mode = PushMake;
                                            scp_stk = scp_stk}
                          IsNotBody inst_nil_ctx) in r
     |~~| seq_postamble

and g_ctrl ctx =
  function
    CtrlSeqIfForm(i) -> g_if ctx i
  | CtrlSeqMatchForm(m) -> g_match ctx m

and g_match ctx =
  let inst = if sw_hungry_if ctx.sw
    then "hmatch"
    else "match"

  in let pattern_counter = Common.counter ()
  in let _UID = uniq64 ()
  in let action_label i =
       Printf.sprintf "%s-p%s" _UID
       @@ tu64 i
  in let match_end_label = _UID -- "end"

  in let _g_pat_and_act =
       function
         PatternAndMatch(ws, w)
         -> let label = action_label @@ pattern_counter ()

         in let new_names = ref []

         in let map_pattern =
              (* COMPLICATED!!!!!! *)
              function
                WName(pn) as w ->
                let n = match pn with
                    NRegular(x) -> x
                  | NTailCall(x) ->
                    (ignore @@ List.map (lookup_name ctx.scp_stk) x); x
                    (* We can't invoke a name that does not exist yet. *)

                in (match n with
                      x::[] ->
                      (try if lookup_name ctx.scp_stk x
                              = Uint64.zero
                              (* This stands for the name does not exist in
                                 debug mode. *)
                         then
                           (new_names := x::!new_names;
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

                g_word {ctx with mode = Pattern}
                  IsNotBody inst_nil_ctx w
              (* I have to log this name into current scope if it does not
                 belong to it, because in this situation, user is binding new
                 names in patterns *)
              | _ as w ->
                g_word {ctx with mode = Pattern}
                  IsNotBody inst_nil_ctx w

         in let ret = cnil
            |~~| let r = aggregate (List.map map_pattern ws) in r
            |~~| cone1 inst (negate_label label)
              (* Jump to the negated label if this pattern doesn't match,
                 otherwise, just continue. *)
            |~~| let r = g_word {ctx with mode = PushMake}
                     IsBody
                     inst_nil_ctx w in r
            |~~| cone1 "jump" match_end_label
            |~~| LabeledCodeSegment([negate_label label], [])
         in List.iter (pop_name ctx.scp_stk) !new_names; ret
         (* I have to remove the new names because they belong only here,
            not the incoming patterns. *)
         (* COMPLICATED!!!!!!! *)

  in function
      PatternsAndMatches(ps)
      -> cnil
         |~~| (aggregate @@ List.map _g_pat_and_act ps)
         |~~| LabeledCodeSegment([match_end_label], [])

and g_if ctx =
  let _UID = uniq64 ()
  in let brancht_id = _UID
  in let branchf_id = negate_label _UID

  in let inst t = if sw_hungry_if ctx.sw
       then Printf.sprintf "h%s %s" t branchf_id
       else Printf.sprintf "%s %s" t branchf_id
       (* If the condition is not met, jump to the false branch. *)

  in let if_body_ = function
      IfBody(bt, bf) ->
       let brancht = g_word ctx IsBody inst_nil_ctx bt
       in let branchf = g_word ctx IsBody inst_nil_ctx bf

       in let if_end_id = Printf.sprintf "%s-end" _UID

       in LabeledCodeSegment([brancht_id], [brancht])
          |~~| cone1 "jump" if_end_id
          |~~| LabeledCodeSegment([branchf_id], [branchf])
          |~~| LabeledCodeSegment([if_end_id], [])

  in let _g_body ib i =
       cnil
       |~~| cone0 i
       |~~| if_body_ ib

  in function
    (* These instruction are generated oppositely for example,
       when you generate for IfGEZ, because we only jump when not GEZ,
       we generate "j not GEZ" => "jlz". *)
    IfGEZ(ib) -> _g_body ib @@ inst "jlz"
  | IfGZ(ib) -> _g_body ib @@ inst "jlez"
  | IfLEZ(ib) -> _g_body ib @@ inst "jgz"
  | IfLZ(ib) -> _g_body ib @@ inst "jgez"
  | IfEZ(ib) -> _g_body ib @@ inst "jnez"
  | IfNEZ(ib) -> _g_body ib @@ inst "jez"
  | IfT(ib) -> _g_body ib @@ inst"jf"
  | IfF(ib) -> _g_body ib @@ inst "jt"
  | IfEmpty(ib) -> _g_body ib @@ "jne"
  | IfNonEmpty(ib) -> _g_body ib @@ "je"
(* je and jne do not have their hungry counter-parts *)

and g_fun ctx inst_ctx =
  let _UID = uniq64 ()
  in let lead_inst =
    match ctx.mode with
      MakeOnly -> "make-fun"
    | PushMake -> "push-fun"
    | Pattern -> "patpush-fun"
    (* I don't think you should write a such complex pattern with a
       anonymous function invocation in it. *)
  in let st_label = _UID -- "st"
  in let end_label = _UID -- "end"
  in let real_end_label = _UID -- "real-end"
  in let preamble = LabeledCodeSegment([st_label],
                                       [cone0 (if sw_share_stack ctx.sw
                                               then "share-stack"
                                               else "push-stack");
                                        cone0 "push-scope"])
  in let scp_stk = push_scope ctx.scp_stk
  in let _g_arg_def = function
        ArgDef(pn)
      | ArgDefWithType(pn, _) ->
        push_name scp_stk pn @@ name_repr_tick ();
        cone1 "fun-arg" @@ tu64 @@ lookup_name scp_stk pn
  in let _g_arg_push = function
        ArgDef(pn)
      | ArgDefWithType(pn, _) ->
        cone1 "push-name" @@ tu64 @@ lookup_name scp_stk pn

  in function
    Function(arg_defs, body)
    -> cnil
       |~~| inst_ctx.pre _UID
       |~~| (cone1 lead_inst @@ st_label)
       |~~| inst_ctx.post _UID
       |~~| preamble
       |~~| let r = csnl (List.map _g_arg_def arg_defs) in r
         (* The bug stated at 383 is also causing problem here leading the
            name pushing code running later than name resolving code. This
            results in failures in names resolving.

            Irrelevantly, for tail calls, its start label is always two
            instructions behind the canonical start label. *)
       |~~| let r = csnl (List.map _g_arg_push @@ List.rev arg_defs) in r
       |~~| (let r = g_word
                 {ctx with mode = PushMake; scp_stk = scp_stk}
                 IsBody
                 inst_nil_ctx body in r)
          (* I think an OCaml bug lies here:
             1. I have checked that infix operators starting with | are
             left-associative, so this expression is evaluated top-down
             rather than the other way around;
             2. So by (1) this "ret" CSeg and the big CSeg before it are
             going to be called as (|~~|) big-CSeg ret-CSeg, but by
             printing all these, I reckon that OCaml calls
             (|~~|) ret-CSeg big-CSeg, so certain labels won't align in
             one line;
             3. By explicitly evaluate this g_word blah blah, I get to
             have (|~~|) big-CSeg ret-CSeg. Hope this isn't
             something I did wrongly.
             4. OCaml is sometimes *super* stupid. *)
       |~~| LabeledCodeSegment([end_label], [cone0 "pop-scope"; cone0 "ret"])
       |~~| LabeledCodeSegment([real_end_label], [])

and g_bind ctx =
  let _g_bind_body =
    function
      BindBody(pn, b) ->
      (push_name ctx.scp_stk pn @@ name_repr_tick ());
      (cnil
      |~~| (g_word
              {ctx with mode = MakeOnly}
              IsNotBody
              {inst_nil_ctx with post = fun uid ->
                   (cone1 "bind"
                    @@ tu64
                    @@ lookup_name ctx.scp_stk pn)
                   |~~| (cone1 "jump" (uid -- "real-end"))}
              b))
  in function
      BindThen(bs, b)
      -> cnil
         |~~| let r = csnl (List.map _g_bind_body bs) in r
         |~~| let r = g_word {ctx with mode = PushMake}
                  IsBody inst_nil_ctx b
                  in r

and g_word ctx is_body_ inst_ctx = function
    WLiteral(pv) -> g_lit ctx inst_ctx pv

  | WName(n) -> g_name ctx n

  | WBackquote(bq) -> g_backquote ctx bq

  | WSequence(seq) -> g_seq
                        ctx
                        is_body_
                        inst_ctx
                        seq

  | WControl(ctrl) -> g_ctrl ctx ctrl

  | WFunction(f) -> g_fun ctx inst_ctx f

  | WBind(b) -> g_bind ctx b

  | WIdle -> cone0 "idle"

  | _ -> cone0 "not-implemented";;

let assemble cst fn sw =
  let scope_stack_init = push_scope []

  in global_fn_id := fn;
  let result = List.map
      (g_word
         {sw = sw; mode = PushMake; scp_stk = scope_stack_init}
         IsBody
         inst_nil_ctx)
      (match cst with
         Sentence(ws) -> ws
       | _ -> [WIdle])

  in (result
      @ [cone0 "terminate"])
     |> aggregate
     |> compose
