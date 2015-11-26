open Tasm_ast;;
open Stdint;;

let lbl:(string, uint64) Hashtbl.t = Hashtbl.create ~random:true 512;;

let replace_label =
  function (* I could have generate this with again. But,... nah... *)
    MATCH(ArgLabel(Label(s)))
    -> MATCH(ArgLit(VUFixedInt(Hashtbl.find lbl s)))
  | HMATCH(ArgLabel(Label(s)))
    -> HMATCH(ArgLit(VUFixedInt(Hashtbl.find lbl s)))

  | JUMP(ArgLabel(Label(s)))
    -> JUMP(ArgLit(VUFixedInt(Hashtbl.find lbl s)))

  | JGEZ(ArgLabel(Label(s)))
    -> JGEZ(ArgLit(VUFixedInt(Hashtbl.find lbl s)))
  | HJGEZ(ArgLabel(Label(s)))
    -> HJGEZ(ArgLit(VUFixedInt(Hashtbl.find lbl s)))
  | JGZ(ArgLabel(Label(s)))
    -> JGZ(ArgLit(VUFixedInt(Hashtbl.find lbl s)))
  | HJGZ(ArgLabel(Label(s)))
    -> HJGZ(ArgLit(VUFixedInt(Hashtbl.find lbl s)))

  | JLEZ(ArgLabel(Label(s)))
    -> JLEZ(ArgLit(VUFixedInt(Hashtbl.find lbl s)))
  | HJLEZ(ArgLabel(Label(s)))
    -> HJLEZ(ArgLit(VUFixedInt(Hashtbl.find lbl s)))
  | JLZ(ArgLabel(Label(s)))
    -> JLZ(ArgLit(VUFixedInt(Hashtbl.find lbl s)))
  | HJLZ(ArgLabel(Label(s)))
    -> HJLZ(ArgLit(VUFixedInt(Hashtbl.find lbl s)))

  | JEZ(ArgLabel(Label(s)))
    -> JEZ(ArgLit(VUFixedInt(Hashtbl.find lbl s)))
  | HJEZ(ArgLabel(Label(s)))
    -> HJEZ(ArgLit(VUFixedInt(Hashtbl.find lbl s)))
  | JNEZ(ArgLabel(Label(s)))
    -> JNEZ(ArgLit(VUFixedInt(Hashtbl.find lbl s)))
  | HJNEZ(ArgLabel(Label(s)))
    -> HJNEZ(ArgLit(VUFixedInt(Hashtbl.find lbl s)))

  | JT(ArgLabel(Label(s)))
    -> JT(ArgLit(VUFixedInt(Hashtbl.find lbl s)))
  | HJT(ArgLabel(Label(s)))
    -> HJT(ArgLit(VUFixedInt(Hashtbl.find lbl s)))
  | JF(ArgLabel(Label(s)))
    -> JF(ArgLit(VUFixedInt(Hashtbl.find lbl s)))
  | HJF(ArgLabel(Label(s)))
    -> HJF(ArgLit(VUFixedInt(Hashtbl.find lbl s)))

  | JE(ArgLabel(Label(s)))
    -> JE(ArgLit(VUFixedInt(Hashtbl.find lbl s)))
  | JNE(ArgLabel(Label(s)))
    -> JNE(ArgLit(VUFixedInt(Hashtbl.find lbl s)))
  | HJE(ArgLabel(Label(s)))
    -> HJE(ArgLit(VUFixedInt(Hashtbl.find lbl s)))
  | HJNE(ArgLabel(Label(s)))
    -> HJNE(ArgLit(VUFixedInt(Hashtbl.find lbl s)))


  | MAKE_FUN(ArgLabel(Label(s)))
    -> MAKE_FUN(ArgLit(VUFixedInt(Hashtbl.find lbl s)))
  | PUSH_FUN(ArgLabel(Label(s)))
    -> PUSH_FUN(ArgLit(VUFixedInt(Hashtbl.find lbl s)))

  | _ as x -> x;;

let rec _inflate cnt =
  let to_strs = List.map (fun x -> match x with Label(l) -> l)
  in let _reg labels rest =
       let none_of_the_labels_are_registered =
         List.fold_left (&&) true
         @@ List.map (fun x -> not (Hashtbl.mem lbl x)) @@ to_strs labels
       in if none_of_the_labels_are_registered
       then let () =
              List.iter (fun x -> Hashtbl.add lbl x cnt) @@ to_strs labels
         in _inflate (Uint64.succ cnt) rest
       else failwith "Redefining labels, exiting."

  in function
      [] -> ()
    | CLine(labels, _)::rest ->
      _reg labels rest
    | Line(labels, _)::rest ->
      _reg labels rest;;

let assemble asm =
  match asm with
    Asm(ls) ->
    let () = _inflate Uint64.zero ls
    in Asm(List.map
             (fun x -> match x with
                  Line(_, inst) -> Line([], replace_label inst)
                | CLine(_, Some(inst)) -> Line([], replace_label inst)
                | CLine(_, None) -> CLine([], None))
             ls)
