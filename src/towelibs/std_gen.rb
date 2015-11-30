require 'stringio'

def gen_make_inline_fun(w, e, st_line_=2, nid_=1)
  st_line = st_line_
  nid = nid_

  lambda do |name, body|
    end_line = body.size + st_line + 4
    w.puts "make-fun #{(st_line + 3).to_s}u"
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

  for i in [['add', '+'],
            ['sub', '-'],
            ['mul', '*'],
            ['div', '/'],
            ['pow', '**']]
    instn, funn = i

    make_inline_fun.call "#{funn}", ["#{instn}"]
  end

  for i in ['fint', 'ufint', 'int', 'float', 'str']
    make_inline_fun.call "To-#{i}", ["to-#{i}"]
  end

  make_inline_fun.call '!print', ['show']
  make_inline_fun.call '!println', ['show', "push-lit '\n'", 'show']
  make_inline_fun.call '!pop', ['pop']
  make_inline_fun.call '!rev', ['reverse']
  make_inline_fun.call '!probe', ['dint']
  make_inline_fun.call '!invoke', ['invoke']

  w.puts 'terminate'

  File.open 'std.w', 'w' do |f| f.puts w.string end
  File.open 'std.e', 'w' do |f| f.puts e.string end
end
