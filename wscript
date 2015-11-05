#! /usr/bin/env python3

import os

APPNAME = 'Towel'
VERSION = '0.0'

top = '.'
out = 'build'

def options(opt):
    opt.add_option('--docs', action='store_true', default=False,
                   dest='compile_docs')
    opt.add_option('--native', action='store_true', default=False,
                   dest='compile_natively')

def configure(ctx):
    ctx.load('ocaml')

    ctx.find_program('ocamlfind', var="OCAMLFIND")
    ret = ctx.exec_command(['ocamlfind', 'query', 'Batteries'])

    def find_lib(l):
        ret = ctx.exec_command([ctx.env.OCAMLFIND[0], 'query', l])
        if ret:
            ctx.fatal('Cannot find library \'%s\'.' % l)
        else:
            ctx.msg('Checking for library \'%s\'' % l, 'ok')

    ctx.env.LIBS = ['Batteries', 'Extlib', 'Stdint', 'Sha']
    for l in ctx.env.LIBS:
        find_lib(l)

    ctx.env.OC = [os.path.basename(ctx.env.OCAMLC[0])]
    ctx.env.NATIVE = False
    if ctx.options.compile_natively:
        ctx.env.NATIVE = True
        ctx.env.OC = [os.path.basename(ctx.env.OCAMLOPT[0])]

    print(ctx.env.OC)

    if ctx.options.compile_docs:
        ctx.load('tex')

def tcp(ctx):
    ctx.recurse('src/compiler', 'build')

def tvm(ctx):
    ctx.recurse('src/vm', 'build')

def docs(ctx):
    ctx.recurse('docs', 'build')

def build(ctx):
    ctx.recurse('src/compiler src/vm')
    if ctx.options.compile_docs:
        ctx.recurse('docs')
