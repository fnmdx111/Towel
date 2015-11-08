
type code_segment =
    LabeledCodeSegment of string list * (* start labels *)
                          code_segment list (* code *)
  | CodeOneliner of string;;

let cone1 inst arg =
  CodeOneliner(String.concat " " [inst; arg]);;

let cone0 inst =
  CodeOneliner(inst);;

let csnl cs = LabeledCodeSegment([], cs);;

let cnil = csnl [];;

let (|>>) x y =
  if x = "" then y
  else if y = "" then x
  else x ^ "\n" ^ y;;

let (|=>) x y =
  if x = "" then y
  else if y = "" then x
  else x ^ " " ^ y;;

let assemble_labels = List.fold_left (|=>) "";;

let (|~~|) cs ocs = match cs, ocs with
    (* TODO NEEDS REWORK since I have fixed the asm parser
       issue on newlines and labels. *)
    LabeledCodeSegment([], []), _
    -> ocs
  | LabeledCodeSegment(st1, []),
    LabeledCodeSegment(st2, csl)
    -> LabeledCodeSegment(st1 @ st2, csl)
  | LabeledCodeSegment(st1, csl1),
    LabeledCodeSegment([], csl2)
    -> let rrest = List.rev @@ List.tl @@ List.rev csl1
    in let rfirst = List.hd @@ List.rev csl1
    in (match rfirst with
          LabeledCodeSegment(l_st, []) ->
          LabeledCodeSegment(st1, rrest @ [LabeledCodeSegment(l_st, csl2)])
        | LabeledCodeSegment(l_st, l_csl) ->
          LabeledCodeSegment(st1, rrest @ [LabeledCodeSegment(l_st,
                                                              l_csl @ csl2)])
        | CodeOneliner(_) ->
          LabeledCodeSegment(st1, csl1 @ csl2))
  | LabeledCodeSegment(st1, csl1),
    LabeledCodeSegment(st2, csl2)
    -> let rrest = List.rev @@ List.tl @@ List.rev csl1
    in let rfirst = List.hd @@ List.rev csl1
    in (match rfirst with
          LabeledCodeSegment(l_st, []) ->
          LabeledCodeSegment(st1, rrest
                                  @ [LabeledCodeSegment(l_st @ st2, csl2)])
        | LabeledCodeSegment(_, _)
        | CodeOneliner(_) ->
          LabeledCodeSegment(st1, csl1 @ [ocs]))
  | LabeledCodeSegment(st1, []),
    CodeOneliner(co)
    -> LabeledCodeSegment(st1, [ocs])
  | LabeledCodeSegment(st1, csl1),
    CodeOneliner(co)
    -> let rrest = List.rev @@ List.tl @@ List.rev csl1
    in let rfirst = List.hd @@ List.rev csl1
    in (match rfirst with
          LabeledCodeSegment(l_st, l_cs) ->
          LabeledCodeSegment(st1, rrest
                                  @ [LabeledCodeSegment(l_st,
                                                        l_cs @ [ocs])])
        | CodeOneliner(_) as lco ->
          LabeledCodeSegment(st1, rrest @ [lco; ocs]))
  | CodeOneliner(_), CodeOneliner(_)
    -> csnl @@ cs::[ocs]
  | CodeOneliner(_), LabeledCodeSegment([], [])
    -> cs
  | CodeOneliner(_), LabeledCodeSegment([], csl)
    -> csnl ([cs] @ csl)
  | CodeOneliner(_), LabeledCodeSegment(_, _)
    -> csnl [cs; ocs]

let aggregate = List.fold_left (|~~|) cnil

let rec comp = function
    CodeOneliner(c) -> c
  | LabeledCodeSegment(st, [])
    -> assemble_labels st
  | LabeledCodeSegment(st, c::cs) ->
    ""
    |>> assemble_labels st |=> comp c
    |>> List.fold_left (|>>) "" @@ List.map comp cs

let rec composite = comp;;

let rec dump_cs = function
    LabeledCodeSegment(ss, css)
    -> Printf.sprintf "start: %s -> [\n%s] dump ends here\n" (assemble_labels ss)
         (String.concat "\n" @@ List.map dump_cs css)
  | CodeOneliner(c)
    -> Printf.sprintf "co: %s" c

