=begin
This is ccg.py (Towel Assembly Parser generator script).

This script accepts two arguments, the first one for output directory, the
second one for the prototype file for scanner.mll.

This script outputs the following files:
  * tasm_ast.mli
  * tasm_parser.mly
  * tasm_scanner.mll
  * tasm_stringify.ml
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

      if Inst::INST_LABELS.include? id
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

type line = Line of label list * inst;;

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
  Line(ls, inst) -> (p_labels ls) ^ " " ^ (p_inst inst);;

let p_asm = function
  Asm(lines) -> String.concat "\n" (List.map p_line lines);;
LINEEND
  end
end

if __FILE__ == $0
  gen_scanner
  gen_parser
  gen_ast
  gen_stringify
end
