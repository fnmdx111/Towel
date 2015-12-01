require 'stringio'

def gen_make_inline_fun(w, e, st_line_=2, nid_=1)
  st_line = st_line_
  nid = nid_

  lambda do |name, body, type|
    end_line = body.size + st_line + 4
    w.puts "push-fun #{(st_line + 3).to_s}u"
    w.puts "bind #{nid}u \"#{name}\""
    w.puts "jump #{end_line}u"
    body.each {|inst| w.puts inst}
    w.puts "shared-ret"

    e.puts "(#{name} #{nid}u #{type})"

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

    make_inline_fun.call("#{funn}", ["#{instn}"],
                         "(" + ["(Int Int Int 1)",
                                "(FInt FInt FInt 1)",
                                "(UFInt UFInt UFInt 1)",
                                "(Float Float Float 1)"].join(" ") + ")")

  end

  types =[['fint', 'FInt'], ['ufint', 'UFInt'],
          ['int', 'Int'], ['float', 'Float'], ['str', 'Str']]
  for i in types
    type_sig = types.reject{|x| x != i}.map{|x| "(#{x[1]} #{i[1]} 1)"}.join " "
    type_sig = "(#{type_sig})"

    make_inline_fun.call".2#{i[0]}", ["to-#{i[0]}"], type_sig
  end

  make_inline_fun.call '!print', ['show'], "((Any 0))"
  make_inline_fun.call '!println', ['show', "push-lit '\n'", 'show'], "((Any 0))"
  make_inline_fun.call '!pop', ['pop'], "((-1))"
  make_inline_fun.call '!rev', ['reverse'], "((UInt 0))"
  make_inline_fun.call '!probe', ['dint'], "((0))"
  make_inline_fun.call '!invoke', ['invoke'], "((Any :var-arg))"
  make_inline_fun.call '.hd', ['car'], "((List Any 1))"
  make_inline_fun.call '.tl', ['cdr'], "((List List 1))"
  make_inline_fun.call '?empty', ['list-empty'], "((List Atom 1))"
  make_inline_fun.call '.t1', ['car'], "((Tuple Any 1) (List Any 1))"
  make_inline_fun.call '.t2', ['cdr', 'car'], "((Tuple Any 1) (List Any 1))"
  make_inline_fun.call '.t3', ['cdr', 'cdr', 'car'], "((Tuple Any 1) (List Any 1))"

  w.puts 'terminate'

  File.open 'std.w', 'w' do |f| f.puts w.string end
  File.open 'std.e', 'w' do |f| f.puts e.string end
end
