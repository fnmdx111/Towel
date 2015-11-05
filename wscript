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
    opt.add_option('--all', action='store_true', default=False,
                   dest='compile_all')

def configure(ctx):
    def conf_ocaml():
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
        if ctx.options.compile_natively:
            ctx.env.OC = [os.path.basename(ctx.env.OCAMLOPT[0])]

    def conf_tex():
        ctx.load('tex')

    if ctx.options.compile_docs:
        conf_tex()
    elif ctx.options.compile_all:
        conf_ocaml()
        conf_tex()
    else:
        conf_ocaml()

def build(ctx):
    if ctx.options.compile_docs:
        ctx.recurse('docs')
    elif ctx.options.compile_all:
        ctx.recurse('src/compiler src/vm')
        ctx.recurse('docs')
    else:
        ctx.recurse('src/compiler src/vm')

