=begin
Definition for the instructions of Towel Assembly Language.

See also the Towel Assembly Language Manual.
=end

require 'set'

module Inst
  nullary_instructions = [
    'push-scope', 'pop-scope',
    'make-backquote',
    'end-list', 'end-tuple',
    'pop',
    'unpack',
    'push-stack', 'share-stack', 'pop-stack',
    'ret', 'shared-ret',
    'show', 'read',
    'dint',
    'type',
    'not-implemented',
    'idle',
    'backquote-name',
    'backquote',
    'patbackquote',
    'terminate',
  ]

  unary_instructions = [
    'bind', 'fun-arg',
    'pack',
    'jump',
    'match',
    'hmatch',
    'rjump',
  ]

  binary_instructions = [
    'push-name', 'make-name', 'patpush-name', 'push-tail-name', 'patpush-tail-name',
    'import-explicit', 'import-implicit'
  ]

  multiarity_instructions = [
  ]

  for i in ['make', 'push', 'patpush']
    unary_instructions.push "#{i}-lit"

    for j in ['list', 'tuple']
      nullary_instructions.push "#{i}-#{j}"
    end
    for j in ['fun']
      unary_instructions.push "#{i}-#{j}"
    end
  end

  inst_that_supports_label_as_arguments = Set.new [
    'jump', 'match', 'hmatch'
  ]

  for i in ['make', 'push', 'patpush']
    inst_that_supports_label_as_arguments.add "#{i}-fun"
  end

  for i in ['gez', 'gz', 'lez', 'lz', 'ez', 'nez', 't', 'f', 'e', 'ne']
    unary_instructions.push "j#{i}"
    inst_that_supports_label_as_arguments.add "j#{i}"

    unary_instructions.push "hj#{i}"
    inst_that_supports_label_as_arguments.add "hj#{i}"
  end

  for i in ['fint', 'ufint', 'int', 'float']
    for j in ['add', 'sub', 'mul', 'div', 'pow', 'equ']
      nullary_instructions.push "#{i}-#{j}"
    end
  end

  for i in ['fint', 'ufint']
    for j in ['and', 'or', 'xor', 'not', 'shl', 'shr', 'shll']
      nullary_instructions.push "#{i}-#{j}"
    end
  end

  NUINST = nullary_instructions
  UNINST = unary_instructions
  BIINST = binary_instructions
  MAINST = multiarity_instructions
  INST_LABELS = inst_that_supports_label_as_arguments
end
