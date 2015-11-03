
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

