=begin
This is ccg.py (Towel Assembly Parser generator script).

This script accepts two arguments, the first one for output directory, the
second one for the prototype file for scanner.mll.

This script outputs the following files:
  * tasm_ast.mli
  * tasm_parser.mly
  * tasm_scanner.mll
  * tasm_stringify.ml
  * tasm_bytecode.ml
  * tasm_inv_bytecode.ml
for a minimal TASM parser.
=end

require_relative 'inst'

NUINST = Inst::NUINST
UNINST = Inst::UNINST
BIINST = Inst::BIINST
MAINST = Inst::MAINST

OUTD = ARGV[0]
SCANNER_P = ARGV[1]

def _open(fn, &block)
  open File.join(OUTD, "tasm_#{fn}"), 'w', &block
end

class String
  def in2id
    self.gsub(/-/, '_').upcase
  end
  def id2in
    self.gsub(/_/, '-').downcase
  end
end

def gen_scanner
  _open 'scanner.mll' do |f|
    wl = lambda {|x| f.write x; f.write "\n"}
    content = open(SCANNER_P).read
    wl.call content

    for inst in NUINST + UNINST + BIINST + MAINST
      wl.call "| \"#{inst}\" { #{inst.in2id} }"
    end

    wl.call "| _ as s {
    failwith (Printf.sprintf \"unexpected character `%c'\" s)
}"
  end
end

def gen_parser
  _open 'parser.mly' do |f|
    wl = lambda {|x| f.write x; f.write "\n"}
    wl.call "%{
open Tasm_ast
%}

%token EOF #{(UNINST + NUINST + BIINST + MAINST).map(&:in2id).join " "}

%token <Tasm_ast.lit> LITERAL
%token <Tasm_ast.label> LABEL

%start asm
%type <Tasm_ast.asm> asm

%%

inst:"

    for i in NUINST
      id = i.in2id
      wl.call "| #{id} { #{id} }"
    end

    for i in UNINST
      id = i.in2id

      if Inst::INST_LABELS.include? i
        wl.call "| #{id} LABEL { #{id}(ArgLabel($2)) }"
      end

      wl.call "| #{id} LITERAL { #{id}(ArgLit($2)) }"
    end

    for i in BIINST
      id = i.in2id
      wl.call "| #{id} LITERAL LITERAL { #{id}(ArgLit($2), ArgLit($3)) }"
    end

    for i in MAINST
      id = i.in2id
      wl.call "| #{id} nonempty_list(LITERAL)
    { #{id}(List.map (fun x -> ArgLit(x)) $2) }
"
    end

    wl.call "line: list(LABEL) inst { Line($1, $2) }

asm: list(line) EOF { Asm($1) }"
  end
end

def gen_ast
  _open 'ast.mli' do |f|
    wl = lambda {|x| f.write x; f.write "\n"}

    gen_defs = lambda {|fmt, defs|
      if defs.empty?
        ''
      else
        defs.map {|x| fmt % x.in2id}.join " | "
      end
    }

    wl.call "open Stdint;;

type lit = VString of string
         | VAtom of uint64
         | VInt of Big_int.big_int
         | VFixedInt of int64
         | VUFixedInt of uint64
         | VFloat of float;;

type label = Label of string;;

type arg = ArgLit of lit
         | ArgLabel of label;;

type inst =
  #{NUINST.map(&:in2id).join " | "}
#{unless UNINST.empty? then " | " else "" end}#{gen_defs.call "%s of arg", UNINST}
#{unless BIINST.empty? then " | " else "" end}#{gen_defs.call "%s of arg * arg", BIINST}
#{unless MAINST.empty? then " | " else "" end}#{gen_defs.call "%s of arg list", MAINST};;

type line = Line of label list * inst
          | CLine of label list * inst option;;

type asm = Asm of line list;;
"
  end
end

def gen_stringify
  _open 'stringify.ml' do |f|
    wl = lambda {|x| f.write x; f.write "\n"}

    wl.call <<LINEEND
open Stdint
open Tasm_ast

let p_label = function Label(l) -> l;;

let p_labels ls = String.concat " " (List.map p_label ls);;

let p_arg = function
    ArgLabel(l) -> p_label l
  | ArgLit(v) -> (match v with
      VString(s) -> Printf.sprintf "'%s'" s
    | VAtom(a) -> Printf.sprintf "%sa" @@ Uint64.to_string a
    | VInt(i) -> Printf.sprintf "%sl" @@ Big_int.string_of_big_int i
    | VFixedInt(i) -> Int64.to_string i
    | VUFixedInt(u) -> Printf.sprintf "%su" @@ Uint64.to_string u
    | VFloat(f) -> string_of_float f);;

let _p_inst inst arg = inst ^ " " ^ (p_arg arg);;
let _p_inst_ba inst arg1 arg2 = inst ^ " " ^ (p_arg arg1) ^ " " ^ (p_arg arg2);;
let _p_inst_ma inst args =
  inst ^ " " ^ (String.concat " " (List.map p_arg args));;
let _p_inst_na inst = inst;;

let p_inst =
  function
  #{NUINST.map {|x| "#{x.in2id} -> _p_inst_na \"#{x}\""}.join " | "}
  #{unless UNINST.empty? then " | " else "" end}#{UNINST.map {|x|
 "#{x.in2id}(arg) -> _p_inst \"#{x}\" arg"}.join " | "}
  #{unless BIINST.empty? then " | " else "" end}#{BIINST.map {|x|
 "#{x.in2id}(arg1, arg2) -> _p_inst_ba \"#{x}\" arg1 arg2"}.join " | "}
  #{unless MAINST.empty? then " | " else "" end}#{MAINST.map {|x|
 "#{x.in2id}(args) -> _p_inst_ma \"#{x}\" args"}.join " | "}

let p_line = function
  Line(ls, inst) -> (p_labels ls) ^ " " ^ (p_inst inst)
| CLine(ls, maybe_inst) -> (p_labels ls) ^
    (match maybe_inst with
       Some(inst) -> " " ^ (p_inst inst)
     | None -> "");;

let p_asm = function
  Asm(lines) -> String.concat "\n" (List.map p_line lines);;
LINEEND
  end
end

NUBIN = Hash[Inst::NUALL]
UNBIN = Hash[Inst::UNALL]
BIBIN = Hash[Inst::BIALL]
MABIN = Hash[Inst::MAALL]

MAGIC_4242 = "BB"
MAGIC_DATA_END = '\2550\128\255'

def gen_bytecode_compiler
  _open 'bytecode.ml' do |f|
    wl = lambda {|x| f.write x; f.write "\n"}

    wl.call <<LINEEND
open Stdint;;
open Tasm_ast;;

let _MAGIC_4242 = Bytes.of_string "#{MAGIC_4242}";;
let _MAGIC_DATA_END = Bytes.of_string "#{MAGIC_DATA_END}";;

let nil = Char.chr 0;;

let data = Hashtbl.create 512;;
let __x () =
  let table = Hashtbl.create 512
  in let _a = [|0|]
  in let tick () = _a.(0) <- _a.(0) + 1; _a.(0)
  in (fun v -> try
         Hashtbl.find table v
       with Not_found ->
         let x = tick ()
         in Hashtbl.replace table v x;
         x),
     (fun v -> Hashtbl.find table v);;

let vid, _ = __x ();;

let mark x = Bytes.cat (Bytes.of_string x);;

let _8B () = Bytes.init 8 (fun _ -> nil);;
let _4B () = Bytes.init 4 (fun _ -> nil);;

let uint64_to_bytes x = let _content = _8B ()
  in Uint64.to_bytes_little_endian x _content 0;
  _content;;

let int64_to_bytes x = let _content = _8B ()
  in Int64.to_bytes_little_endian x _content 0;
  _content;;

let string_to_bytes x = let _len = _4B ()
  in Uint32.to_bytes_little_endian (Uint32.of_int (String.length x))
    _len 0;
  Bytes.cat _len (Bytes.of_string x);;

let store =
  let save bytes =
    let id_ = vid bytes
    in (if Hashtbl.mem data id_
        then () else Hashtbl.replace data id_ bytes);
    id_
  in function
    ArgLabel(l) -> failwith "Please assemble the IR first."
  | ArgLit(v) -> (match v with
        VString(s)
        -> let str_b = mark "s" (string_to_bytes s)
        in save str_b
      | VAtom(a)
        -> let atom_b = mark "a" (uint64_to_bytes a)
        in save atom_b
      | VInt(i)
        -> let int_b = mark "i" (i
                                 |> Big_int.string_of_big_int
                                 |> string_to_bytes)
        in save int_b
      | VFixedInt(i)
        -> let fint_b = mark "f" (int64_to_bytes i)
        in save fint_b
      | VUFixedInt(u)
        -> let ufint_b = mark "u" (uint64_to_bytes u)
        in save ufint_b
      | VFloat(f)
        -> let float_b = mark "n" (* stands for NaN *)
               (f |> string_of_float |> string_to_bytes)
        in save float_b);;

let _b_inst_na x = uint64_to_bytes (Uint64.of_int x);;

let _b_inst x arg_ =
  let _inst_bin = _b_inst_na x
  in let arg_id = store arg_
  in let _content = _8B ()
  in let _t = uint64_to_bytes (Uint64.of_int arg_id)
  in Bytes.blit _inst_bin 0 _content 0 1;
  Bytes.blit _t 0 _content 1 7;
  _content;;

let _b_inst_ba x arg1 arg2 =
  let _inst_bin = _b_inst_na x
  in let arg1_id = store arg1
  in let arg2_id = store arg2
  in let _content = _8B ()
  in let _1 = uint64_to_bytes (Uint64.of_int arg1_id)
  in let _2 = uint64_to_bytes (Uint64.of_int arg2_id)
  in Bytes.blit _inst_bin 0 _content 0 1;
  Bytes.blit _1 0 _content 1 4;
  Bytes.blit _2 0 _content 5 3;
  _content;;

let b_inst =
  function
  #{NUINST.map {|x| "#{x.in2id} -> _b_inst_na #{NUBIN[x]}"}.join " | "}
  #{unless UNINST.empty? then " | " else "" end}#{UNINST.map {|x|
 "#{x.in2id}(arg) -> _b_inst #{UNBIN[x]} arg"}.join " | "}
  #{unless BIINST.empty? then " | " else "" end}#{BIINST.map {|x|
 "#{x.in2id}(arg1, arg2) -> _b_inst_ba #{BIBIN[x]} arg1 arg2"}.join " | "}
  #{unless MAINST.empty? then " | " else "" end}#{MAINST.map {|x|
 "#{x.in2id}(args) -> _b_inst_ma #{MABIN[x]} args"}.join " | "}

let b_line = function
  Line(_, inst) -> b_inst inst
| CLine(ls, maybe_inst) ->
    (match maybe_inst with
       Some(inst) -> b_inst inst
     | None -> Bytes.of_string "");;

let b_asm = function
    Asm(lines) ->
    let lines_b = (List.fold_left (fun acc x -> Bytes.cat acc x)
                     (Bytes.of_string "")
                     (List.map b_line lines))
    in let data_seg =
      let sorted_data =
        List.sort (fun x y -> Pervasives.compare (fst x) (fst y))
        @@ Hashtbl.fold (fun k v acc -> (k, v)::acc) data []
      in List.fold_left (fun acc x -> Bytes.cat acc (snd x))
        _MAGIC_4242
        sorted_data
    in Bytes.cat
      (Bytes.cat data_seg _MAGIC_DATA_END)
      lines_b;;
LINEEND
  end
end

def gen_inv_bytecode
  _open 'inv_bytecode.ml' do |f|
    wl = lambda {|x| f.write x; f.write "\n"}

    wl.call <<LINEEND
open Stdint;;
open Tasm_ast;;
open Batteries;;
open BatIO;;
open BatEnum;;


let next_byte_str input = input |> read_byte |> char_of_int |> String.make 1;;

let next_string_raw input n =
  fold (fun acc _ -> acc ^ (next_byte_str input)) "" (1 -- n);;

let next_int input =
  let xs = next_string_raw input 4 |> Bytes.of_string
  in Uint32.of_bytes_little_endian xs 0 |> Uint32.to_int;;

let next_u64 input =
  let xs = next_string_raw input 8 |> Bytes.of_string
  in Uint64.of_bytes_little_endian xs 0;;

let next_string input =
  let n = next_int input
  in next_string_raw input n;;

let recover_data input =
  let __tick = [|0|]
  in let tick () = __tick.(0) <- __tick.(0) + 1; __tick.(0)

  in let rec next_data acc =
    let indicator = next_byte_str input
    in if indicator = "\\255"
    then if next_string_raw input 3 = "0\\128\\255"
      then acc
      else failwith "Unknown data end magic."
    else let arg = match indicator with
          "s" -> ArgLit(VString(next_string input))
        | "a" -> ArgLit(VAtom(next_u64 input))
        | "i" -> ArgLit(VInt(input |> next_string |> Big_int.of_string))
        | "f" -> ArgLit(VFixedInt(next_u64 input |> Uint64.to_int64))
        | "u" -> ArgLit(VUFixedInt(next_u64 input))
        | "n" -> ArgLit(VFloat(next_string input |> float_of_string))
        | _ -> failwith "Corrupted bytecode."
      in Hashtbl.replace acc (tick ()) arg;
      next_data acc

  in next_data (Hashtbl.create 512);;

let _OPCODE_MASK = Uint64.of_int 0xff;;
let _ARG_SHIFT = 8;;
let _ARG1_SHIFT = 8;;
let _ARG2_SHIFT = 40;;
(*
   0 -- 7|8 -- 15|16 -- 23|24 -- 31|32 -- 39|40 -- 47|48 -- 55|56 -- 63
   opcode unused
   opcode arg
   opcode arg1                               arg2
 *)

let inv_b_inst data i =
  let opcode = Uint64.to_int (Uint64.logand i _OPCODE_MASK)
  in let iarg = Uint64.shift_right_logical i _ARG_SHIFT |> Uint64.to_int
  in let iarg1 = Uint64.shift_right_logical i _ARG1_SHIFT |> Uint64.to_int
  in let iarg2 = Uint64.shift_right_logical i _ARG2_SHIFT |> Uint64.to_int
  in let retrieve i = Hashtbl.find data i

  in match opcode with
    #{NUINST.map {|x| "#{NUBIN[x]} -> #{x.in2id}"}.join " | "}
 #{unless UNINST.empty? then " | " else "" end}#{UNINST.map {|x|
"#{UNBIN[x]} -> #{x.in2id}(retrieve iarg)"}.join " | "}
 #{unless BIINST.empty? then " | " else "" end}#{BIINST.map {|x|
"#{BIBIN[x]} -> #{x.in2id}(retrieve iarg1, retrieve iarg2)"}.join " | "}
  | _ -> failwith "Unrecognized opcode.";;

let parse_bytecode inchan =
  let input = input_channel inchan
  in let insts = ref []
  in if next_string_raw input 2 = "BB"
  then let data = recover_data input
    in let rec __x () =
         try let b_inst = next_u64 input
           in insts := (Line([], inv_b_inst data b_inst))::(!insts);
           __x ()
         with BatIO.No_more_input ->
           insts := List.rev (!insts);
    in __x (); Array.of_list (!insts)
  else failwith "Invalid Towel bytecode file.";;
LINEEND
  end
end

if __FILE__ == $0
  gen_scanner
  gen_parser
  gen_ast
  gen_stringify
  gen_bytecode_compiler
  gen_inv_bytecode
end
