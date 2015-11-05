open Ast
open Scoping
open Batteries
open Switches
open Cseg
open Stdint

let global_fn_id = ref "";;

type word_builder_mode =
    MakeOnly
  | PushMake
  | Pattern;;

type is_body =
    IsNotBody
  | IsBody;;

type is_shared_sequence =
    IsSharedSeq
  | IsNotSharedSeq;;

let __unique64 = Common.counter ();;
let uniq64 x = Printf.sprintf ":%s-%s" !global_fn_id @@ Int64.to_string @@ __unique64 x;;

let negate_label l = l ^ "!";;

let flap = flip List.map;;

let atom_dict = Hashtbl.create 512;;
let atom_repr_tick = Common.counter ();;

ignore @@ atom_repr_tick ();; (* tick for false *)
ignore @@ atom_repr_tick ();; (* tick for true *)

let name_repr_tick = Common.counter ();;

let fun_tick = Common.counter ();;

let (--) x y = Printf.sprintf "%s-%s" x y;;

(* All the g_* functions should return a code_segment value. *)
let rec g_lit
    sw
    mode
    scp_stk
    cs_pre_leading_inst
    cs_post_leading_inst
    lit =
  let inst t =
    (match mode with
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
    in cone1 (inst "atom") @@ Int64.to_string repr

  | VFixedInt(i) ->
    cone1 (inst "fint") @@ Int64.to_string i

  | VUFixedInt(u) ->
    cone1 (inst "ufint") @@ Uint64.to_string u

  | VInt(i) ->
    cone1 (inst "int") @@ Printf.sprintf "%sl" @@ Big_int.string_of_big_int i

  | VFloat(f) ->
    cone1 (inst "float") @@ string_of_float f

  | VList(wl)
    -> cnil (* ascii art!! *) 
       |~~| cs_pre_leading_inst
       |~~| cone0 @@ inst "list"
       |~~| cs_post_leading_inst
       |~~| csnl (flap wl @@ g_word sw PushMake IsNotBody scp_stk
                    cnil cnil)
       |~~| cone0 "end-list"

  | VString(s) ->
    cone1 (inst "string") @@ Printf.sprintf "'%s'" s

  | VTuple(wl)
    -> cnil
       |~~| cs_pre_leading_inst
       |~~| (cone0 @@ inst "tuple")
       |~~| cs_post_leading_inst
       |~~| csnl (flap wl @@ g_word sw PushMake IsNotBody scp_stk
                    cnil cnil)
       |~~| cone0 "end-tuple"

  | VAlTypeLiteral(_) ->
    cone0 "not-implemented"

and g_name mode scp_stk pn =
  let to_name_id_string ns =
    String.concat " "
    @@ List.map (fun x -> Printf.sprintf "0x%sL" @@ Int64.to_string x)
    @@ List.map (lookup_name scp_stk) ns
  in let prefix = match mode with
        PushMake -> ""
      | Pattern -> "pat"
      | MakeOnly -> "" (* What's the point in this? *)
  in let inst t = prefix ^ t -- "name"
  in let ns, t = match pn with
        NRegular(n) -> n, "push"
      | NTailCall(n) -> n, "push-tail"
  in cone1 (inst t) (to_name_id_string ns)

and g_backquote sw mode scp_stk =
  let bq_inst = Printf.sprintf "%sbackquote" (match mode with
        MakeOnly -> "make-"
      | PushMake -> ""
      | Pattern -> "pat")
  in function
    BQValue(pv) ->
    g_lit sw mode scp_stk cnil (cone0 bq_inst) pv

    | BQName(n)
      -> cnil
         |~~| cone0 "backquote-name"
         |~~| g_name mode scp_stk n

  | BQSeq(seq) ->
    g_seq sw mode IsNotBody scp_stk cnil (cone0 bq_inst) seq

  | BQBackquote(b) ->
    g_backquote sw mode scp_stk b

and g_seq sw mode is_body_ scp_stk
    cs_pre_leading_inst cs_post_leading_inst
    seq =
  let _UID = uniq64 ();
  in let seq_st_id = Printf.sprintf "%s-st" _UID
  in let seq_end_id = Printf.sprintf "%s-end" _UID
  in let blpt what =
       if sw_opt_seq sw
       then what
       else match is_body_ with
           IsNotBody -> what
         | IsBody -> cnil

  in let seq_preamble =
       let _x = match seq with
           Sequence(_) -> "push"
         | SharedSequence(_) -> "share"
       in let _p = LabeledCodeSegment([seq_st_id],
                                      [cone0 @@ _x -- "scope";
                                       cone0 @@ _x -- "stack"])
       in blpt _p

  in let seq_postamble = blpt (LabeledCodeSegment([seq_end_id],
                                                  [cone0 "ret"]))

  in let body, scp_stk = match seq with
        Sequence(s) -> s, push_scope scp_stk
      | SharedSequence(s) -> s, scp_stk

  in let lead_inst = match mode with
        MakeOnly -> cone1 "make-fun" seq_st_id
      | PushMake -> cone1 "push-fun" seq_st_id
      | Pattern -> cone1 "patpush-fun" seq_st_id

  in cnil
     |~~| cs_pre_leading_inst
     |~~| lead_inst
     |~~| cs_post_leading_inst
     |~~| seq_preamble
     |~~| csnl (flap body @@ g_word sw mode IsNotBody scp_stk
                        cnil cnil)
     |~~| seq_postamble

and g_ctrl sw mode scp_stk =
  function
    CtrlSeqIfForm(i) -> g_if sw mode scp_stk i
  | CtrlSeqMatchForm(m) -> g_match sw mode scp_stk m

and g_match sw mode scp_stk =
  let inst = if sw_hungry_if sw
    then "hmatch"
    else "match"

  in let pattern_counter = Common.counter ()
  in let _UID = uniq64 ()
  in let action_label i =
       Printf.sprintf "%s-p%s" _UID
       @@ Int64.to_string i
  in let match_end_label = _UID -- "end"

  in let _g_pat_and_act =
       function
         PatternAndMatch(ws, w)
         -> let label = action_label @@ pattern_counter ()
         in cnil
            |~~| aggregate (flap ws
                            @@ g_word sw Pattern IsNotBody scp_stk
                              cnil cnil)
            |~~| cone1 inst (negate_label label)
            |~~| let r = g_word sw PushMake IsBody scp_stk cnil cnil w
            in r
            |~~| cone1 "jump" match_end_label
            |~~| LabeledCodeSegment([negate_label label], [])

  in function
      PatternsAndMatches(ps)
      -> cnil
         |~~| (aggregate @@ List.map _g_pat_and_act ps)
         |~~| LabeledCodeSegment([match_end_label], [])

and g_if sw mode scp_stk =
  let inst t = if sw_hungry_if sw
    then Printf.sprintf "h%s" t
    else t
  in let if_body_ = function
      IfBody(bt, bf) ->
       let brancht = g_word sw mode IsBody scp_stk cnil cnil bt
       in let branchf = g_word sw mode IsBody scp_stk cnil cnil bf
       in let _UID = uniq64 ()
       in let brancht_id = _UID
       in let branchf_id = negate_label _UID
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
    IfGEZ(ib) -> _g_body ib @@ inst "jgez"
  | IfGZ(ib) -> _g_body ib @@ inst "jgz"
  | IfLEZ(ib) -> _g_body ib @@ inst "jlez"
  | IfLZ(ib) -> _g_body ib @@ inst "jlz"
  | IfEZ(ib) -> _g_body ib @@ inst "jez"
  | IfNEZ(ib) -> _g_body ib @@ inst "jnez"
  | IfT(ib) -> _g_body ib @@ inst"jt"
  | IfF(ib) -> _g_body ib @@ inst "jf"
  | IfEmpty(ib) -> _g_body ib @@ "je"
  | IfNonEmpty(ib) -> _g_body ib @@ "jne"
(* je and jne do not have their hungry counter-parts *)

and g_fun sw mode
    scp_stk
    cs_pre_leading_inst cs_post_leading_inst =
  let _UID = uniq64 ()
  in let lead_inst =
    match mode with
      MakeOnly -> "make-fun"
    | PushMake -> "push-fun"
    | Pattern -> "patpush-fun"
  in let st_label = _UID -- "st"
  in let end_label = _UID -- "end"
  in let preamble = LabeledCodeSegment([st_label],
                                       [cone0 "push-scope";
                                        cone0 (if sw_share_stack sw
                                               then "share-stack"
                                               else "push-stack")])
  in let scp_stk = push_scope scp_stk
  in let _g_arg_def = function
        ArgDef(pn)
      | ArgDefWithType(pn, _) ->
        push_name scp_stk pn @@ name_repr_tick ();
        cone1 "fun-arg" @@ Int64.to_string @@ lookup_name scp_stk pn
  in function
    Function(arg_defs, body)
    -> cnil
       |~~| cs_pre_leading_inst
       |~~| (cone1 lead_inst @@ st_label)
       |~~| cs_post_leading_inst
       |~~| preamble
       |~~| csnl (List.map _g_arg_def arg_defs)
       |~~| let r = g_word sw mode IsBody scp_stk cnil cnil body in r
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
       |~~| LabeledCodeSegment([end_label], [cone0 "ret"])

and g_bind sw mode scp_stk =
  let _g_bind_body =
    function
      BindBody(pn, b) ->
      push_name scp_stk pn @@ name_repr_tick ();
      cnil
      |~~| g_word sw MakeOnly IsNotBody scp_stk cnil
        (cone1 "bind"
         @@ Int64.to_string
         @@ lookup_name scp_stk pn)
        b
  in function
      BindThen(bs, b)
      -> cnil
         |~~| csnl (List.map _g_bind_body bs)
         |~~| g_word sw PushMake IsBody scp_stk cnil cnil b

and g_word sw wb_mode is_body_ scp_stk pre post = function
    WLiteral(pv) -> g_lit sw wb_mode scp_stk pre post pv

  | WName(n) -> g_name wb_mode scp_stk n

  | WBackquote(bq) -> g_backquote sw wb_mode scp_stk bq

  | WSequence(seq) -> g_seq
                        sw (* global compiler switch *)
                        wb_mode
                        is_body_
                        scp_stk
                        pre post
                        seq

  | WControl(ctrl) -> g_ctrl sw wb_mode scp_stk ctrl

  | WFunction(f) -> g_fun sw wb_mode scp_stk pre post f

  | WBind(b) -> g_bind sw wb_mode scp_stk b

  | WIdle -> cone0 "idle"

  | _ -> cone0 "not-implemented";;

let assemble cst fn sw =
  let scope_stack_init = push_scope []

  in global_fn_id := fn;
  let result = List.map
         (g_word sw PushMake IsBody scope_stack_init cnil cnil)
         (match cst with
            Sentence(ws) -> ws
          | _ -> [WIdle])

  in result
     |> aggregate
     |> composite

