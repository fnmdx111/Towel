open Tasm_ast

let lbl = Hashtbl.create ~random:true 512;;

let replace_label =
  function (* I could have generate this with Python3 again. But,... nah... *)
    MATCH(ArgLabel(Label(s)))
    -> MATCH(ArgLit(VFixedInt(Hashtbl.find lbl s)))
  | HMATCH(ArgLabel(Label(s)))
    -> HMATCH(ArgLit(VFixedInt(Hashtbl.find lbl s)))

  | JUMP(ArgLabel(Label(s)))
    -> JUMP(ArgLit(VFixedInt(Hashtbl.find lbl s)))

  | JGEZ(ArgLabel(Label(s)))
    -> JGEZ(ArgLit(VFixedInt(Hashtbl.find lbl s)))
  | HJGEZ(ArgLabel(Label(s)))
    -> HJGEZ(ArgLit(VFixedInt(Hashtbl.find lbl s)))
  | JGZ(ArgLabel(Label(s)))
    -> JGZ(ArgLit(VFixedInt(Hashtbl.find lbl s)))
  | HJGZ(ArgLabel(Label(s)))
    -> HJGZ(ArgLit(VFixedInt(Hashtbl.find lbl s)))

  | JLEZ(ArgLabel(Label(s)))
    -> JLEZ(ArgLit(VFixedInt(Hashtbl.find lbl s)))
  | HJLEZ(ArgLabel(Label(s)))
    -> HJLEZ(ArgLit(VFixedInt(Hashtbl.find lbl s)))
  | JLZ(ArgLabel(Label(s)))
    -> JLZ(ArgLit(VFixedInt(Hashtbl.find lbl s)))
  | HJLZ(ArgLabel(Label(s)))
    -> HJLZ(ArgLit(VFixedInt(Hashtbl.find lbl s)))

  | JEZ(ArgLabel(Label(s)))
    -> JEZ(ArgLit(VFixedInt(Hashtbl.find lbl s)))
  | HJEZ(ArgLabel(Label(s)))
    -> HJEZ(ArgLit(VFixedInt(Hashtbl.find lbl s)))
  | JNEZ(ArgLabel(Label(s)))
    -> JNEZ(ArgLit(VFixedInt(Hashtbl.find lbl s)))
  | HJNEZ(ArgLabel(Label(s)))
    -> HJNEZ(ArgLit(VFixedInt(Hashtbl.find lbl s)))

  | JT(ArgLabel(Label(s)))
    -> JT(ArgLit(VFixedInt(Hashtbl.find lbl s)))
  | HJT(ArgLabel(Label(s)))
    -> HJT(ArgLit(VFixedInt(Hashtbl.find lbl s)))
  | JF(ArgLabel(Label(s)))
    -> JF(ArgLit(VFixedInt(Hashtbl.find lbl s)))
  | HJF(ArgLabel(Label(s)))
    -> HJF(ArgLit(VFixedInt(Hashtbl.find lbl s)))

  | JE(ArgLabel(Label(s)))
    -> JE(ArgLit(VFixedInt(Hashtbl.find lbl s)))
  | JNE(ArgLabel(Label(s)))
    -> JNE(ArgLit(VFixedInt(Hashtbl.find lbl s)))

  | MAKE_FUN(ArgLabel(Label(s)))
    -> MAKE_FUN(ArgLit(VFixedInt(Hashtbl.find lbl s)))
  | PUSH_FUN(ArgLabel(Label(s)))
    -> PUSH_FUN(ArgLit(VFixedInt(Hashtbl.find lbl s)))
  | PATPUSH_FUN(ArgLabel(Label(s)))
    -> PATPUSH_FUN(ArgLabel(Label(s)))

  | _ as x -> x;;

let rec _inflate cnt =
  let to_strs = List.map (fun x -> match x with Label(l) -> l)
  in function
    [] -> ()
  | Line(labels, inst)::rest
    -> let none_of_the_labels_are_registered =
         List.fold_left (&&) true
         @@ List.map (fun x -> not (Hashtbl.mem lbl x)) @@ to_strs labels
    in if none_of_the_labels_are_registered
    then let () =
           List.iter (fun x -> Hashtbl.add lbl x cnt) @@ to_strs labels
      in _inflate (Int64.succ cnt) rest
    else failwith "Redefining labels, exiting.";;

let unlabel asm =
  match asm with
    Asm(ls) ->
    let () = _inflate Int64.zero ls
    in Asm(List.map
             (fun x -> match x with
                  Line(_, inst) -> Line([], replace_label inst))
             ls)
