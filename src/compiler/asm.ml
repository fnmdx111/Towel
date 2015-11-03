open Ast
open Scoping
open Batteries
open Vcounter
open Switches
open Cseg

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

let scope_stack = init_scope_stack;;
let _push_name = push_name scope_stack;;
let _pop_scope = pop_scope scope_stack;;
let _push_scope = push_scope scope_stack;;
let _lookup_name = lookup_name scope_stack;;

let negate_label l = l ^ "!";;

let flap = flip List.map;;

let atom_dict = Hashtbl.create ~random:true 512;;
let atom_repr_tick = Common.counter ();;

let name_repr_tick = Common.counter ();;

let fun_tick = Common.counter ();;

let (--) x y = Printf.sprintf "%s-%s" x y;;

(* All the g_* functions should return a code_segment value. *)
let rec g_lit
    sw
    mode
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
    in cone1 (inst "atom") @@ string_of_int repr

  | VFixedInt(i) ->
    cone1 (inst "fint") @@ string_of_int i

  | VFloat(f) ->
    cone1 (inst "float") @@ string_of_float f

  | VList(wl) ->
    (* ascii art!! *) cnil
    |~~| cs_pre_leading_inst
    |~~| LabeledCodeSegment(None, None, [cone0 @@ inst "list"])
    |~~| cs_post_leading_inst
    |~~| LabeledCodeSegment(None, None,
                            flap wl
                            @@ g_word sw PushMake IsNotBody)
    |~~| cone0 "end-list"

  | VString(s) ->
    cone1 (inst "string") @@ Printf.sprintf "'%s'" s

  | VTuple(wl) -> cnil
    |~~| cs_pre_leading_inst
    |~~| LabeledCodeSegment(None, None, [cone0 @@ inst "tuple"])
    |~~| cs_post_leading_inst
    |~~| LabeledCodeSegment(None, None,
                            flap wl
                            @@ g_word sw PushMake IsNotBody)
    |~~| cone0 "end-tuple"

  | VAlTypeLiteral(_) ->
    cone0 "not-implemented"

and g_name pn =
  let to_name_id_string ns =
    String.concat " "
    @@ List.map (Printf.sprintf "0x%x")
    @@ List.map _lookup_name ns
  in let inst t = t -- "name"
  in let ns, t = match pn with
        NRegular(n) -> (n, "push")
      | NTailCall(n) -> (n, "push-tail")
  in cone1 (inst t) @@ to_name_id_string ns

and g_backquote sw mode vc bq =
  let bq_inst = Printf.sprintf "%sbackquote" (match mode with
        MakeOnly -> "make-"
      | PushMake -> ""
      | Pattern -> "pat")
  in match bq with
    BQValue(pv) ->
    g_lit sw mode cnil (cone0 bq_inst) pv

  | BQName(n) -> cnil
    |~~| cone0 "backquote-name"
    |~~| g_name n

  | BQSeq(seq) ->
    g_seq sw mode IsNotBody (vc_seq_inc vc) cnil (cone0 bq_inst) seq

  | BQBackquote(b) ->
    g_backquote sw mode vc b

and g_seq sw mode is_body_ vc
    cs_pre_leading_inst cs_post_leading_inst
    seq =
  let seq_st_id = Printf.sprintf "%s-st" @@ vc_to_label vc
  in let seq_end_id = Printf.sprintf "%s-end" @@ vc_to_label vc
  in let test_and_set what =
       if sw_opt_seq sw
       then what
       else match is_body_ with
           IsNotBody -> what
         | IsBody -> cnil

  in let seq_preamble =
       let _x = match seq with
           Sequence(_) -> _push_scope; "push"
         | SharedSequence(_) -> "shared"
       in let _p = LabeledCodeSegment(Some(seq_st_id),
                                      Some(seq_end_id),
                                      [cone0 @@ _x -- "scope";
                                       cone0 @@ _x -- "stack"])
       in test_and_set _p

  in let seq_postamble = test_and_set @@ cone0 "ret-seq"

  in let lead_inst = match mode with
        MakeOnly -> cone0 "make-seq"
      | PushMake -> cone0 "push-seq"
      | Pattern -> cone0 "patpush-seq"

  in let body = match seq with
        Sequence(s) -> s
      | SharedSequence(s) -> s

  in cnil
     |~~| cs_pre_leading_inst
     |~~| lead_inst
     |~~| cs_post_leading_inst
     |~~| seq_preamble
     |~~| CodeSegment(flap body @@ g_word sw mode IsNotBody vc)
     |~~| seq_postamble

and g_ctrl sw mode vc ctrl =
  match ctrl with
    CtrlSeqIfForm(i) -> g_if sw mode (vc_if_inc vc) i
  | CtrlSeqMatchForm(m) -> g_match sw mode (vc_match_inc vc) m

and g_match sw mode vc ctrl =
  let inst = if sw_hungry_if sw
    then "hmatch"
    else "match"

  in let pattern_counter = Common.counter ()
  in let match_label = vc_to_label vc
  in let action_label i = Printf.sprintf "%s-p%d" match_label i
  in let match_end_label = match_label -- "end"

  in let _g_pat_and_act paa =
       match paa with
         PatternAndMatch(ws, w)
         -> let label = action_label @@ pattern_counter ()
         in cnil
            |~~| CodeSegment(flap ws
                             @@ g_word sw Pattern IsNotBody vc)
            |~~| cone1 inst label
            |~~| LabeledCodeSegment(Some(label), None,
                                    [g_word sw PushMake IsBody vc w])
            |~~| cone1 "jump" match_end_label
            |~~| LabeledCodeSegment(Some(negate_label label), None, [])

  in match ctrl with
    PatternsAndMatches(ps) -> cnil
                              |~~| CodeSegment(List.map _g_pat_and_act ps)
                              |~~| LabeledCodeSegment(Some(match_end_label),
                                                      None, [])

and g_if sw mode vc ctrl =
  let inst t = if sw_hungry_if sw
    then Printf.sprintf "h%s" t
    else t
  in let if_body_ = function
      IfBody(bt, bf) ->
       let brancht = g_word sw mode IsBody vc bt
       in let branchf = g_word sw mode IsBody vc bf
       in let label = vc_to_label vc
       in let brancht_id = label
       in let branchf_id = negate_label label
       in let if_end_id = Printf.sprintf "%s-end" label
       in LabeledCodeSegment(Some(brancht_id), None, [brancht])
          |~~| cone1 "jump" if_end_id
          |~~| LabeledCodeSegment(Some(branchf_id), None, [branchf])
          |~~| LabeledCodeSegment(Some(if_end_id), None, [])

  in let _g_body ib i =
       cnil
       |~~| cone0 i
       |~~| if_body_ ib

  in match ctrl with
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

and g_fun sw mode vc cs_pre_leading_inst cs_post_leading_inst =
  let lead_inst =
    match mode with
      MakeOnly -> "make-fun"
    | PushMake -> "push-fun"
    | Pattern -> "patpush-fun"
  in let st_label = vc_to_label vc -- "st"
  in let end_label = vc_to_label vc -- "end"
  in let preamble = LabeledCodeSegment(Some(st_label),
                                       None,
                                       [cone0 "push-scope";
                                        cone0 (if sw_share_stack sw
                                               then "share-stack"
                                               else "push-stack")])
  in let _g_arg_def = function
        ArgDef(pn) ->
        _push_name pn @@ name_repr_tick ();
        cone1 "fun-arg" @@ string_of_int @@ _lookup_name pn
      | ArgDefWithType(pn, _) ->
        _push_name pn @@ name_repr_tick ();
        cone1 "fun-arg" @@ string_of_int @@ _lookup_name pn
  in _push_scope; function
    Function(arg_defs, body)
    -> cnil
       |~~| cs_pre_leading_inst
       |~~| (cone1 lead_inst @@ string_of_int @@ vc_fun vc)
       |~~| cs_post_leading_inst
       |~~| preamble
       |~~| CodeSegment(List.map _g_arg_def arg_defs)
       |~~| g_word sw mode IsBody vc body
       |~~| LabeledCodeSegment(Some(end_label), None, [cone0 "ret"])


and g_word sw wb_mode is_body_ vc = function
    WLiteral(pv) -> g_lit sw wb_mode cnil cnil pv

  | WName(n) -> g_name n

  | WBackquote(bq) -> g_backquote sw wb_mode vc bq

  | WSequence(seq) -> g_seq
                        sw (* global compiler switch *)
                        wb_mode
                        is_body_
                        (vc_seq_inc vc) (* various label counter *)
                        cnil cnil
                        seq

  | WControl(ctrl) -> g_ctrl sw wb_mode vc ctrl

  | WFunction(f) -> g_fun sw wb_mode (vc_fun_inc vc) cnil cnil f

  | WBind(b) -> g_bind sw wb_mode vc b

  | _ -> cone0 "not-implemented";;


let _ =
  let lexbuf = Lexing.from_channel stdin

  in try
    let cst = Parser.sentence Scanner.token lexbuf

    in let sw = CompilerSwitches(false, false, true)

    in let result = List.map
        (g_word sw PushMake IsBody vc_zero)
        (match cst with Sentence(ws, _) -> ws)

    in print_string (composite result)

  with
  | Exc.LexicalError(s, ln, b) ->
    Printf.printf "(%d,%d) Lexical error: %s.\n" ln b s
  | Exc.SyntacticError(s, ln, st, e) ->
    Printf.printf "(%d,%d-%d) Syntactic error: %s.\n"
      ln st e s
