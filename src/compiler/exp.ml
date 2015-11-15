open Stdint;;
open Ast;;
open Scoping;;

(* ==========================================
     Export file parser
   ========================================== *)

let exp_filename fn =
  String.concat "." [fn; "e"];;

let parse cst =
  let parse_one acc =
    function
      WSequence(Sequence(ws)) ->
      let name =
        match List.hd ws with
          WName(NRegular(ns)) -> List.hd ns
        | _ -> failwith "WTF a non name word?"

      in let idx =
           match List.hd (List.tl ws) with
             WLiteral(x) ->
             (match x.value_content with
                VUFixedInt(u) -> u
              | _ -> failwith "WTF a non ufixedint as index?")
           | _ -> failwith "WTF a non literal as index?"

      in Hashtbl.replace acc name.name_repr idx; acc
    (* I apologize for so much nested match expressions. :( *)
    | _ -> failwith "Unrecognized structure in .E file."

  in match cst with
    Sentence(ws) -> List.fold_left parse_one
                      (Hashtbl.create 512)
                      ws

let open_export impn =
  let lexbuf = Lexing.from_channel @@ Pervasives.open_in impn
  in let cst = Parser.sentence Scanner.token lexbuf
  in parse cst;;

