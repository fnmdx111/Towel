# This file generates raw wrapper functions for the instructions.
# Most of the raw wrappers have their corresponding partial
# applicable high-level wrappers in module Std.
# However, wrappers for some of the stack instructions are not
# high-level available, because they are designed to work on
# primitive stacks (`pop', for example).

require 'stringio'

def gen_make_inline_fun(w, e, st_line_=2, nid_=1)
  st_line = st_line_
  nid = nid_

  lambda do |name, body|
    end_line = body.size + st_line + 4
    w.puts "push-fun #{(st_line + 3).to_s}u"
    w.puts "bind #{nid}u \"#{name}\""
    w.puts "jump #{end_line}u"
    body.each {|inst| w.puts inst}
    w.puts "shared-ret"

    e.puts "(#{name} #{nid}u)"

    st_line = end_line
    nid += 1
  end
end

if __FILE__ == $0
  w = StringIO.new
  e = StringIO.new

  make_inline_fun = gen_make_inline_fun w, e

  w.puts 'push-scope'
  w.puts 'push-stack'

  for i in [['add', '..+'],
            ['sub', '..-'],
            ['mul', '..*'],
            ['div', '../'],
            ['pow', '..**'],
            ['mod', '..%'],
            ['equ', '..='],
            ['and', '..and'],
            ['or', '..or']]
    instn, funn = i

    make_inline_fun.call "#{funn}", ["#{instn}"]
  end

  types =[['fint', 'FInt'], ['ufint', 'UFInt'],
          ['int', 'Int'], ['float', 'Float'], ['str', 'Str']]
  for i in types
    make_inline_fun.call"..2#{i[0]}", ["to-#{i[0]}"]
  end

  make_inline_fun.call '.!print', ['show']
  make_inline_fun.call '.!println', ['show', "push-lit '\n'", 'show']
  make_inline_fun.call '.!read', ['read']
  make_inline_fun.call '.!dup', ['dup']
  make_inline_fun.call '.!pop', ['pop']
  make_inline_fun.call '.!rev', ['reverse']
  make_inline_fun.call '.!probe', ['dint']
  make_inline_fun.call '.!invoke', ['invoke']
  make_inline_fun.call '.!unpack', ['unpack']
  make_inline_fun.call '.!pack', ['pack']
  make_inline_fun.call '..hd', ['car']
  make_inline_fun.call '..tl', ['cdr']
  make_inline_fun.call '..cons', ['cons']
  make_inline_fun.call '.?empty', ['list-empty']
  make_inline_fun.call '..t1', ['car']
  make_inline_fun.call '..t2', ['cdr', 'car']
  make_inline_fun.call '..t3', ['cdr', 'cdr', 'car']

  make_inline_fun.call '!>ext', ['load-ext']
  make_inline_fun.call '!>>', ['extcall']

  make_inline_fun.call '.idle', ['idle']
  make_inline_fun.call '$$', ['push-phony']

  w.puts 'terminate'

  File.open '.w.o', 'w' do |f| f.puts w.string end
  File.open '.w.e', 'w' do |f| f.puts e.string end
end
