#! /usr/bin/env python3

"""
This is ccg.py (Towel Assembly Parser generator script).

This script accepts two arguments, the first one for output directory, the
second one for the prototype file for scanner.mll.

This script outputs the following files:
  * tasm_ast.mli
  * tasm_parser.mly
  * tasm_scanner.mll
  * tasm_stringify.ml
for a minimal TASM parser.
"""

from inst import nullary_instructions as NUINST
from inst import unary_instructions as UNINST
from inst import binary_instructions as BIINST
from inst import multiarity_instructions as MAINST
from inst import inst_that_supports_label_as_argument

import sys
import os

output_dir = sys.argv[1]

scanner_p = sys.argv[2]

def _open(fn):
    return open(os.path.join(output_dir, 'tasm_%s' % fn), 'w')


def _in2id(i):
    return i.replace('-', '_').upper()

def _id2in(i):
    return i.replace('_', '-').lower()

def gen_scanner():
    with open(scanner_p, 'r') as fi, _open('scanner.mll') as fo:
        print(fi.read(), file=fo)

        for inst in NUINST:
            print('| "%s" { %s }' % (inst, _in2id(inst)),
                  file=fo)

        for inst in UNINST:
            print('| "%s" { %s }' % (inst, _in2id(inst)),
                  file=fo)

        for inst in BIINST:
            print('| "%s" { %s }' % (inst, _in2id(inst)),
                  file=fo)

        for inst in MAINST:
            print('| "%s" { %s }' % (inst, _in2id(inst)),
                  file=fo)

        print('''| _ as s {
    failwith
             (Printf.sprintf "unexpected character `%c'" s)
}''', file=fo)

def gen_parser():
    with _open('parser.mly') as fo:
        print('''%%{
open Tasm_ast
%%}

%%token EOF %s

%%token <Tasm_ast.lit> LITERAL
%%token <Tasm_ast.label> LABEL

%%start asm
%%type <Tasm_ast.asm> asm

%%%%

inst:
''' % (' '.join(map(_in2id, UNINST + NUINST + BIINST + MAINST))),
              file=fo)
        for i in NUINST:
            id_ = _in2id(i)
            print('| %s { %s }' % (id_, id_), file=fo)

        for i in UNINST:
            id_ = _in2id(i)

            if i in inst_that_supports_label_as_argument:
                print('| %s LABEL { %s(ArgLabel($2)) }' % (id_, id_),
                      file=fo)

            print('| %s LITERAL { %s(ArgLit($2)) }' % (id_, id_), file=fo)

        for i in BIINST:
            id_ = _in2id(i)
            print('| %s LITERAL LITERAL { %s(ArgLit($2), ArgLit($3)) }'
                  % (id_, id_),
                  file=fo)

        for i in MAINST:
            id_ = _in2id(i)
            print('| %s nonempty_list(LITERAL)'
                  ' { %s(List.map (fun x -> ArgLit(x)) $2) }' % (id_, id_),
                  file=fo)

        print('''
line: list(LABEL) inst { Line($1, $2) }

asm: list(line) EOF { Asm($1) }
''', file=fo)

def gen_ast():
    def gen_defs(fmt, defs):
        if not defs:
            return ''
        else:
            return '| ' + ' | '.join(map(lambda x: fmt % _in2id(x), defs))

    with _open('ast.mli') as fo:
        print('''open Stdint

type lit = VString of string
         | VInt of Big_int.big_int
         | VFixedInt of int64
         | VUFixedInt of uint64
         | VFloat of float;;

type label = Label of string;;

type arg = ArgLit of lit
         | ArgLabel of label;;

type inst =
%s
%s
%s
%s;;

type line = Line of label list * inst;;

type asm = Asm of line list;;
''' % (' | '.join(map(_in2id, NUINST)),
       gen_defs('%s of arg', UNINST),
       gen_defs('%s of arg * arg', BIINST),
       gen_defs('%s of arg list', MAINST)),
              file=fo)

def gen_stringify():
    p = '''open Stdint
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
  {NU_INST}
{UN_INST}
{BI_INST}
{MA_INST}

let p_line = function
  Line(ls, inst) -> (p_labels ls) ^ " " ^ (p_inst inst);;

let p_asm = function
  Asm(lines) -> String.concat "\n" (List.map p_line lines);;
'''

    NU_INST_U = '{ID} -> _p_inst_na "{IN}"'
    NU_INSTS = ' | '.join(map(lambda x: NU_INST_U.format(ID=_in2id(x), IN=x),
                              NUINST))
    UN_INST_U = '{ID}(arg) -> _p_inst "{IN}" arg'
    UN_INSTS = ' | '.join(map(lambda x: UN_INST_U.format(ID=_in2id(x), IN=x),
                              UNINST))
    if UN_INSTS:
        UN_INSTS = '| ' + UN_INSTS

    BI_INST_U = '{ID}(arg1, arg2) -> _p_inst_ba "{IN}" arg1 arg2'
    BI_INSTS = ' | '.join(map(lambda x: BI_INST_U.format(ID=_in2id(x), IN=x),
                              BIINST))
    if BI_INSTS:
        BI_INSTS = '| ' + BI_INSTS

    MA_INST_U = '{ID}(args) -> _p_inst_ma "{IN}" args'
    MA_INSTS = ' | '.join(map(lambda x: MA_INST_U.format(ID=_in2id(x), IN=x),
                              MAINST))
    if MA_INSTS:
        MA_INSTS = '| ' + MA_INSTS

    with _open('stringify.ml') as fo:
        print(p.format(NU_INST=NU_INSTS, UN_INST=UN_INSTS,
                       BI_INST=BI_INSTS,
                       MA_INST=MA_INSTS),
              file=fo)


if __name__ == '__main__':
    gen_scanner()
    gen_parser()
    gen_ast()
    gen_stringify()
