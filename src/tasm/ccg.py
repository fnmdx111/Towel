#! /usr/bin/env python3

"""
This is ccg.py (Towel Assembly Parser generator script).

This script accepts two arguments, the first one for output directory, the
second one for the prototype file for scanner.mll.

This script outputs three files:
  * ast.mli
  * parser.mly
  * scanner.mll
for a minimal TASM parser.
"""

from inst import nullary_instructions as NUINST
from inst import unary_instructions as UNINST
from inst import multiarity_instructions as MAINST
from inst import inst_that_supports_label_as_argument

import sys
import os

output_dir = sys.argv[1]

scanner_p = sys.argv[2]

def _open(fn):
    return open(os.path.join(output_dir, fn), 'w')


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
open Ast
%%}

%%token EOF %s

%%token <Ast.lit> LITERAL
%%token <Ast.label> LABEL

%%start asm
%%type <Ast.asm> asm

%%%%

inst:
''' % (' '.join(map(_in2id, UNINST + NUINST + MAINST))),
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

        for i in MAINST:
            id_ = _in2id(i)
            print('| %s list(LITERAL)'
                  ' { %s(List.map (fun x -> ArgLit(x)) $2) }' % (id_, id_),
                  file=fo)

        print('''
line: list(LABEL) inst { Line($1, $2) }

asm: list(line) EOF { Asm($1) }
''', file=fo)

def gen_ast():
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
| %s
| %s;;

type line = Line of label list * inst;;

type asm = Asm of line list;;
''' % (' | '.join(map(_in2id, NUINST)),
       ' | '.join(map(lambda x: '%s of arg' % _in2id(x), UNINST)),
       ' | '.join(map(lambda x: '%s of arg list' % _in2id(x), MAINST))),
              file=fo)

if __name__ == '__main__':
    gen_scanner()
    gen_parser()
    gen_ast()
