#! /usr/bin/env python3

import os
import waflib

APPNAME = 'Towel'
VERSION = '-1.-1'

top = '.'
out = 'build'

def options(opt):
    opt.add_option('--docs', action='store_true', default=False,
                   dest='compile_docs')
    opt.add_option('--native', action='store_true', default=False,
                   dest='compile_natively')
    opt.add_option('--tasm', action='store_true', default=False,
                   dest='compile_tasm')
    opt.add_option('--all', action='store_true', default=False,
                   dest='compile_all')
    opt.add_option('--debug', action='store_true', default=False,
                   dest='compile_debug')
    opt.add_option('--conf-test', action='store_true', default=False,
                   dest='conf_test')

def configure(ctx):
    def conf_ocaml():
        ctx.load('ocaml')

        ctx.find_program('ocamlfind', var="OCAMLFIND")
        ctx.find_program('python3', var='PY3K')
        ret = ctx.exec_command(['ocamlfind', 'query', 'Batteries'])

        def find_lib(l):
            ret = ctx.exec_command([ctx.env.OCAMLFIND[0], 'query', l])
            if ret:
                ctx.fatal('Cannot find library \'%s\'.' % l)
            else:
                ctx.msg('Checking for library \'%s\'' % l, 'ok')

        ctx.env.LIBS = {'Batteries', 'Extlib', 'Stdint', 'Sha'}
        ctx.env.TASM_LIBS = {'Stdint'}
        for l in ctx.env.LIBS |\
        (ctx.env.TASM_LIBS if ctx.options.compile_tasm else set()):
            find_lib(l)

        ctx.env.DEBUG = '-g' if ctx.options.compile_debug else ''

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

    if ctx.options.conf_test:
        ctx.find_program('ruby')
        ctx.find_program('gem')

        def test_lib(what):
            ret = ctx.cmd_and_log(['gem', 'list', what],
                                  output=waflib.Context.STDOUT,
                                  quiet=waflib.Context.BOTH)
            if what in ret:
                ctx.msg('Checking for library \'%s\'' % what, 'ok')
            else:
                ctx.fatal('Cannot find library \'%s\'.' % what)

        test_lib('colorize')

def test(ctx):
    ctx.recurse('tests')

def build(ctx):
    if ctx.options.compile_docs:
        ctx.recurse('docs')
    elif ctx.options.compile_all:
        ctx.recurse('src/compiler src/tasm src/vm')
        ctx.recurse('docs')
    else:
        if ctx.options.compile_tasm:
            ctx.recurse('src/tasm')
        else:
            ctx.recurse('src/compiler')

