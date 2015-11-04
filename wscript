#! /usr/bin/env python3

APPNAME = 'Towel'
VERSION = '0.0'

top = '.'
out = 'build'

def options(opt):
    opt.add_option('--docs', action='store_true', default=False,
                   dest='compile_docs')

def configure(ctx):
    ctx.load('ocaml')

    ctx.find_program('ocamlfind', var="OCAMLFIND")
    ret = ctx.exec_command(['ocamlfind', 'query', 'Batteries'])
    if ret:
        ctx.fatal('Cannot find library \'Batteries\'.')
    else:
        ctx.msg('Checking for library \'Batteries\'', 'ok')

    ret = ctx.exec_command([ctx.env.OCAMLFIND[0], 'query', 'Extlib'])
    if ret:
        ctx.fatal('Cannot find library \'Extlib\'.')
    else:
        ctx.msg('Checking for library \'Extlib\'', 'ok')

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
