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
    opt.add_option('--compiler', action='store_true', default=False,
                   dest='compile_compiler_stub')
    opt.add_option('--native', action='store_true', default=False,
                   dest='compile_natively')
    opt.add_option('--tvm', action='store_true', default=False,
                   dest='compile_tvm')
    opt.add_option('--all', action='store_true', default=False,
                   dest='compile_all')
    opt.add_option('--debug', action='store_true', default=False,
                   dest='compile_debug')
    opt.add_option('--conf-test', action='store_true', default=False,
                   dest='conf_test')
    opt.add_option('--no-docs', action='store_true', default=False,
                   dest='no_docs')

def configure(ctx):
    ctx.options.compile_compiler = True

    if ctx.options.compile_docs:
        ctx.options.compile_compiler = False

    if ctx.options.compile_tvm:
        ctx.options.compile_compiler = False

    if ctx.options.no_docs:
        ctx.options.compile_docs = False

    if ctx.options.compile_compiler_stub:
        ctx.options.compile_compiler = True

    if ctx.options.compile_all:
       ctx.options.compile_docs = True
       ctx.options.compile_compiler = True
       ctx.options.compile_tvm = True
       ctx.options.conf_test = True

    def conf_ocaml(programs, libs):
        ctx.load('ocaml')

        ctx.find_program('ocamlfind', var='OCAMLFIND')
        for p, v in programs:
            ctx.find_program(p, var=v)

        def find_lib(l):
            ret = ctx.exec_command([ctx.env.OCAMLFIND[0], 'query', l])
            if ret:
                ctx.fatal('Cannot find library \'%s\'.' % l)
            else:
                ctx.msg('Checking for library \'%s\'' % l, 'ok')

        ctx.env.LIBS = {'Batteries', 'Extlib', 'Stdint', 'Sha'}
        ctx.env.TVM_LIBS = {'Stdint', 'Batteries', 'Extlib', 'Dynlink'}
        for l in libs:
            find_lib(l)

        ctx.env.OC = [os.path.basename(ctx.env.OCAMLC[0])]
        if ctx.options.compile_natively:
            ctx.env.OC = [os.path.basename(ctx.env.OCAMLOPT[0])]

        ctx.env.DEBUG = ''
        if ctx.options.compile_debug:
            ctx.env.DEBUG = '-g'
            ctx.env.OC = [os.path.basename(ctx.env.OCAMLC[0])]

    def conf_tex():
        ctx.load('tex')

    def conf_test():
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

    def conf_compiler():
        conf_ocaml([('ruby', 'RUBY')],
                   ['Batteries', 'Stdint', 'Extlib', 'Sha'])

    def conf_tvm():
        conf_ocaml([('ruby', 'RUBY')], ['Batteries', 'Stdint'])

    if ctx.options.compile_docs:
        conf_tex()
    
    if ctx.options.compile_all:
        conf_compiler()
        conf_tvm()
        conf_tex()
        conf_test()

    if ctx.options.compile_compiler:
        conf_compiler()

    if ctx.options.compile_tvm:
        conf_tvm()

    if ctx.options.conf_test:
        conf_test()

def test(ctx):
    ctx.recurse('tests')

def build(ctx):
    if ctx.options.compile_docs:
        ctx.recurse('docs')

    if ctx.options.compile_tvm:
        ctx.recurse('src/vm')

    if ctx.options.compile_compiler:
        ctx.recurse('src/compiler')

    if ctx.options.compile_all:
        ctx.recurse('src/compiler src/vm')
        ctx.recurse('docs')

