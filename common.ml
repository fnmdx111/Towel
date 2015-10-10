open Ast

(* counters *)
(* I think they might be useful, but I still am not able to find out
   even one of them :( *)
let value_counter =
    let cnt = Array.of_list [-1]
    in fun () -> cnt.(0) <- cnt.(0) + 1; cnt.(0);;

let name_counter =
    let cnt = Array.of_list [-1]
    in fun () -> cnt.(0) <- cnt.(0) + 1; cnt.(0);;

(* build canonical module name out of paths *)
let module_name_from_path p =
  let path_delim = if Sys.os_type = "Win32" then "\\" else "/" in
  let full_fn = List.hd (List.rev (Str.split (Str.regexp path_delim) p)) in
  let file_name = List.hd (Str.split (Str.regexp ".") full_fn) in
  Printf.sprintf "module__%s" file_name;;

let promote_name_to_module_name n =
  n.name_type <- TypeDef([TDPrimitiveType(PT_Module)]);
  n.name_domain <- Ast.MetaModule;;

let module_from_path p =
  let module_name = module_name_from_path p in
  { module_name = { name_repr = module_name;
                    name_type = TypeDef([TDPrimitiveType(PT_Module)]);
                    name_ref_key = -42;
                    name_domain = Ast.MetaModule };
    module_path = p };;

module P = Printf;;

let rec atom_stringify a = P.sprintf "(atom %s %d)" a.atom_name a.atom_repr
and name_stringify a = P.sprintf "(name %s of %s %d)"
    a.name_repr (match a.name_domain with
          MetaModule -> "-META-MODULE"
        | SomeModule(x) -> x.module_name.name_repr) a.name_ref_key
and int_stringify a = P.sprintf "(int-lit %d)" a
and float_stringify a = P.sprintf "(float-lit %f)" a
and string_stringify a = P.sprintf "(str-lit %s)" a
and lit_stringify = function
  VAtom(a) -> atom_stringify a
| VFixedInt(i) -> int_stringify i
| VFloat(f) -> float_stringify f
| VList(l) -> list_stringify l
| VString(ss) -> string_stringify ss

and seq_stringify seq =
  match seq with
    Sequence(ws) -> String.concat ", " (List.map word_stringify ws)

and backquote_stringify a = P.sprintf "(bq-lit %s)"
    (match a with
       BQValue(pv) -> lit_stringify pv.value_content
     | BQName(n) -> name_stringify n
     | BQSeq(seq) -> seq_stringify seq
     | BQBackquote(bq) -> backquote_stringify bq)

and cs_stringify cs =
  let if_stringify s body =
    match body with
      IfBody(w1, w2) -> P.sprintf "%s { %s; %s }" s
                          (word_stringify w1)
                          (word_stringify w2)
  in let pattern_stringify p =
       match p with
         PatternAndMatch(p, m) ->
         P.sprintf "pattern %s -> %s;"
           (words_stringify p)
           (word_stringify m)
  in match cs with
    CtrlSeqIfForm(i) ->
    (match i with
       IfGEZ(ib) -> if_stringify "gez" ib
     | IfGZ(ib) -> if_stringify "gz" ib
     | IfLEZ(ib) -> if_stringify "lez" ib
     | IfLZ(ib) -> if_stringify "lz" ib
     | IfEmpty(ib) -> if_stringify "empty" ib
     | IfNonEmpty(ib) -> if_stringify "non-empty" ib
     | IfEZ(ib) -> if_stringify "ez" ib
     | IfNEZ(ib) -> if_stringify "nez" ib
     | IfT(ib) -> if_stringify "t" ib
     | IfF(ib) -> if_stringify "f" ib)
  | CtrlSeqMatchForm(m) ->
    (match m with
       PatternsAndMatches(ps) -> String.concat "; "
                                   (List.map pattern_stringify ps))

and type_def_item_stringify i =
  match i with
    TDName(n) -> name_stringify n
  | TDPrimitiveType(p) ->
    (match p with
       PT_Any -> "PT_Any"
     | PT_Atom -> "PT_Atom"
     | PT_List -> "PT_List"
     | PT_Float -> "PT_Float"
     | PT_String -> "PT_String"
     | PT_Module -> "PT_Module"
     | PT_FixedInt -> "PT_FixedInt")

and type_def_stringify d =
  match d with
    TypeDef(tds) -> String.concat " -> "
                      (List.map type_def_item_stringify tds)
    
and arg_def_stringify d =
    match d with
      ArgDef(n) -> name_stringify n
    | ArgDefWithType(n, td) ->
      P.sprintf "(%s: %s)" (name_stringify n) (type_def_stringify td)

and of_stringify o =
  match o with
    Bind(n, w) ->
    String.concat " = " [name_stringify n; word_stringify w]
  | BindIn(n, w1, w2) ->
    P.sprintf "%s = %s in %s"
      (name_stringify n) (word_stringify w1) (word_stringify w2)
  | Function(ds, w) ->
    P.sprintf "fun %s = %s"
      (String.concat "; " (List.map arg_def_stringify ds))
      (word_stringify w)
  | Import(w) ->
    P.sprintf "import %s" (word_stringify w)
  | At(w1, w2) ->
    P.sprintf "%s@%s" (word_stringify w1) (word_stringify w2)

and word_stringify w =
  let _w s n = P.sprintf "(%s %s)" s n in
  match w with
    WLiteral(pv) -> _w (lit_stringify pv.value_content) "literal"
  | WName(n) -> _w (name_stringify n) "name"
  | WBackquote(bq) -> _w (backquote_stringify bq) "bquote"
  | WSequence(seq) -> _w (seq_stringify seq) "seq"
  | WControl(cs) -> _w (cs_stringify cs) "cs"
  | WOtherForm(o) -> _w (of_stringify o) "other"

and words_stringify ws =
  String.concat "/" (List.map word_stringify ws)

and list_stringify lss =
  let rec _strf s ls =
    match ls with
      [] -> s
    | w::ws ->
      _strf (String.concat " | " [(word_stringify w); s]) ws
  in _strf "" lss;;

