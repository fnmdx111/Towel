require 'stringio'

def gen_make_inline_fun(w, e, st_line_=0, nid_=1)
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

    st_line += 3
    nid += 1
  end
end

if __FILE__ == $0
  w = StringIO.new
  e = StringIO.new

  make_inline_fun = gen_make_inline_fun w, e

  w.puts 'push-scope'

  for i in [['fint', ''], ['ufint', 'u'], ['float', 'f'], ['int', 'i']]
    ctg, pfix = i
    for j in [['add', '+'],
              ['sub', '-'],
              ['mul', '*'],
              ['div', '/'],
              ['pow', '**'],
              ['equ', '=']]
      instn, funn = j

      make_inline_fun.call "#{funn}#{pfix}", ["#{ctg}-#{instn}"]
    end
  end

  make_inline_fun.call '!print', ['show']

  w.puts 'terminate'

  puts w.string
  puts e.string
end
