open Tasm_ast;;

let make_labels =
  let labelize x = Label(x)
  in List.map labelize;;

let cnil = Asm([CLine([], None)]);;
let line x = Asm([Line([], x)]);;
let lline lbs x = Asm([Line(make_labels lbs, x)]);;
let cline lbs inst = Asm([CLine(make_labels lbs, inst)]);;
let put_label lbs = cline lbs None;;

let (|~~|) a1 a2 =
  let lr = List.rev
  in let lh = List.hd
  in let lt = List.tl
  in let lfirstn xs = xs |> lr |> lt |> lr
  in match a1, a2 with
    Asm([]), Asm(_) ->
    a2 (* If a1 is empty, who cares about a1? *)
  | Asm(ls1), Asm([]) ->
    a1
  | Asm(ls1), Asm(ls2) ->
    let last1 = lh (lr ls1)
    in let first2 = lh ls2
    in let reject_empty = List.filter
           (fun x -> (Pervasives.compare x (CLine([], None))) <> 0)
    in let content = match last1, first2 with
          CLine([], None), CLine([], None) ->
          (* 0 + 0 = 0 *)
          (lfirstn ls1) @ (lt ls2)
        | Line(_, _), Line(_, _) ->
          (* non-0 + non-0 = non-0 *)
          ls1 @ ls2
        | _, CLine([], None) ->
          (* non-0 + 0 = non-0 *)
          ls1 @ (lt ls2)
        | CLine([], None), _ ->
          (* 0 + non-0 = non-0 *)
          (lfirstn ls1) @ ls2
        | Line([], inst1), CLine(lbs2, Some(inst2)) ->
          (* Transform all the CLine into Line. *)
          ls1 @ ((Line(lbs2, inst2))::(lt ls2))
        | Line(_, _), CLine([], Some(inst2)) ->
          (* Can't merge. *)
          ls1 @ ((Line([], inst2))::(lt ls2))
        | Line(_, _), CLine(lbs2, Some(inst2)) ->
          (* Can't merge. *)
          ls1 @ ((Line(lbs2, inst2))::(lt ls2))
        | Line(lbs1, inst1), CLine(lbs2, None) ->
          (* Can't merge. Can't transform. *)
          ls1 @ ls2
        | CLine([], Some(inst1)), Line(_, _) ->
          (lfirstn ls1) @ [Line([], inst1)] @ ls2
        | CLine(lbs1, None), Line([], inst2) ->
          (lfirstn ls1) @ [Line(lbs1, inst2)] @ (lt ls2)
        | CLine(lbs1, None), Line(lbs2, inst2) ->
          (lfirstn ls1) @ [Line(lbs1 @ lbs2, inst2)] @ (lt ls2)
        | CLine(lbs1, None), CLine(lbs2, None) ->
          (lfirstn ls1) @ [CLine(lbs1 @ lbs2, None)] @ (lt ls2)
        | CLine(lbs1, None), CLine(lbs2, Some(inst2)) ->
          (lfirstn ls1) @ [Line(lbs1 @ lbs2, inst2)] @ (lt ls2)
        | CLine(lbs1, Some(inst1)), Line(_, _) ->
          (lfirstn ls1) @ [Line(lbs1, inst1)] @ ls2
        | CLine(lbs1, Some(inst1)), CLine(lbs2, Some(inst2)) ->
          (lfirstn ls1) @ [Line(lbs1, inst1); Line(lbs2, inst2)] @ (lt ls2)
        | CLine(lbs1, Some(inst1)), CLine(lbs2, None) ->
          (lfirstn ls1) @ [Line(lbs1 @ lbs2, inst1)] @ (lt ls2)
    in Asm(reject_empty content)
