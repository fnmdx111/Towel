
type code_segment =
    LabeledCodeSegment of string option * (* start label *)
                          string option * (* end label *)
                          code_segment list (* code *)
  | CodeSegment of code_segment list
  | CodeOneliner of string;;

let cone1 inst arg =
  CodeOneliner(String.concat " " [inst; arg]);;

let cone0 inst =
  CodeOneliner(inst);;

let cnil = CodeSegment([]);;

let (|~~|) cs ocs = match cs, ocs with
    LabeledCodeSegment(st_label, end_label, csl),
    LabeledCodeSegment(_, _, other_csl)
    -> LabeledCodeSegment(st_label, end_label, csl @ other_csl)
  | LabeledCodeSegment(st_label, end_label, csl),
    CodeSegment(other_csl)
    -> LabeledCodeSegment(st_label, end_label, csl @ other_csl)
  | LabeledCodeSegment(st_label, end_label, csl),
    CodeOneliner(_)
    -> LabeledCodeSegment(st_label, end_label, csl @ [ocs])
  | CodeSegment([]), _
    -> ocs
  | CodeSegment(csl), _
    -> CodeSegment(csl @ [ocs])
  | CodeOneliner(_), CodeOneliner(_)
    -> CodeSegment(cs::[ocs])
  | CodeOneliner(_), CodeSegment(cs_)
    -> CodeSegment(cs::cs_)
  | CodeOneliner(_), LabeledCodeSegment(_, _, _)
    -> CodeSegment([cs; ocs]);;

let (|>>) x y =
  if x = "" then y
  else if y = "" then x
  else x ^ "\n" ^ y;;

let (|=>) x y =
  if x = "" then y
  else if y = "" then x
  else x ^ " " ^ y;;

let rec comp =
  let comp_cs = function
      CodeSegment(cs) -> List.fold_left
                           (fun acc x -> acc |>> comp x) "" cs
    |_ -> ""

  in let unpack = function
        Some(x) -> x
      | None -> ""

  in function
    CodeOneliner(c) -> c
  | CodeSegment(_) as cs -> comp_cs cs
  | LabeledCodeSegment(st, end_, []) -> unpack st |=> unpack end_
  | LabeledCodeSegment(st, end_, c::[]) ->
    unpack st |=> unpack end_ |=> comp c
  | LabeledCodeSegment(st, end_, c::cs) ->
    let first = c
    in let last = List.hd @@ List.rev cs
    in let cs_body = List.rev @@ List.tl @@ List.rev cs
    in ""
       |>> unpack st |=> comp first
       |>> List.fold_left (|>>) "" @@ List.map comp_cs cs_body
       |>> unpack end_ |=> comp last

