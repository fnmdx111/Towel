=begin
Definition for the instructions of Towel Assembly Language.

See also the Towel Assembly Language Manual.
=end

require 'set'

module Inst
  nullary_instructions_all = [
    ['push-scope', 0x01],
    ['pop-scope', 0x02],
    ['share-scope', 0x03],

    ['end-list', 0x13], ['end-tuple', 0x14],
    ['pop', 0x18], ['push-phony', 0x1d],
    ['car', 0x5e], ['cdr', 0x5f], ['list-empty', 0x61],
    ['cons', 0x60],
    ['unpack', 0x1b],
    ['push-stack', 0x20], ['share-stack', 0x21], ['pop-stack', 0x22],
    ['ret', 0x24], ['shared-ret', 0x25],
    ['show', 0x67], ['read', 0x68],
    ['dint', 0x71],
    ['type', 0x72],
    ['not-implemented', 0xff],
    ['idle', 0x0],
    ['terminate', 0xfe],
    ['push-lnil', 0x11],
    ['push-tnil', 0x12],
    ['reverse', 0x1a],
    ['pack', 0x1c],
    ['invoke', 0x27],
    ['add', 0x50], ['sub', 0x51], ['mul', 0x52],
    ['div', 0x53], ['pow', 0x54], ['mod', 0x55], ['equ', 0x56],
    ['and', 0x57], ['or', 0x58], ['xor', 0x59], ['not', 0x5a],
    ['shl', 0x5b], ['shr', 0x5c], ['lshr', 0x5d], ['dup', 0x19],
    ['load-ext', 0xf1], ['extcall', 0xf0],
    ['install', 0xf2], ['tuple-at', 0xf3],
  ]

  unary_instructions_all = [
    ['bind', 0x04], ['fun-arg', 0x05],
    ['jump', 0x30],
    ['match', 0xb10],
    ['hmatch', 0xb11],
    ['push-lit', 0x15],
    ['call', 0x28],
    ['push-fun', 0x10],
    ['closure', 0x26],
    ['import', 0x70],
    ['export', 0x7f],
  ]

  binary_instructions_all = [
    ['eval-and-push', 0x17],
    ['push-name', 0x16],
    ['eval-tail', 0x23],
  ]

  multiarity_instructions_all = []

  multiarity_instructions = [
  ]

  inst_that_supports_label_as_arguments = Set.new [
    'jump', 'match', 'hmatch'
  ]

  for i in ['push']
    inst_that_supports_label_as_arguments.add "#{i}-fun"
  end
  inst_that_supports_label_as_arguments.add "call"

  for s in [['gez', 1],
            ['gz', 2],
            ['lez', 3],
            ['lz', 4],
            ['ez', 5],
            ['nez', 6],
            ['t', 7],
            ['f', 8],
            ['e', 9],
            ['ne', 10]]
    i, j = s
    unary_instructions_all.push ["j#{i}", 0x30 + j]
    inst_that_supports_label_as_arguments.add "j#{i}"

    unary_instructions_all.push ["hj#{i}", 0x40 + j]
    inst_that_supports_label_as_arguments.add "hj#{i}"
  end

  for s in [['fint', 2], ['ufint', 3], ['int', 4], ['float', 5], ['str', 6]]
    i, j = s
    nullary_instructions_all.push ["to-#{i}", 0x60 + j]
  end

  nullary_instructions = nullary_instructions_all.map {|x| x[0]}
  nullary_instructions_bin = nullary_instructions_all.map {|x| x[1]}

  unary_instructions = unary_instructions_all.map {|x| x[0]}
  unary_instructions_bin = unary_instructions_all.map {|x| x[1]}

  binary_instructions = binary_instructions_all.map {|x| x[0]}
  binary_instructions_bin = binary_instructions_all.map {|x| x[1]}

  NUINST = nullary_instructions
  UNINST = unary_instructions
  BIINST = binary_instructions
  MAINST = multiarity_instructions
  NUALL = nullary_instructions_all
  UNALL = unary_instructions_all
  BIALL = binary_instructions_all
  MAALL = multiarity_instructions_all
  INST_LABELS = inst_that_supports_label_as_arguments
end
