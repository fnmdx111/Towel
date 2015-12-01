=begin
Definition for the instructions of Towel Assembly Language.

See also the Towel Assembly Language Manual.
=end

require 'set'

module Inst
  nullary_instructions = [
    'push-scope', 'pop-scope',
    'end-list', 'end-tuple',
    'pop', 'push-phony',
    'list-hd', 'list-tl',
    'unpack',
    'push-stack', 'share-stack', 'pop-stack',
    'ret', 'shared-ret',
    'show', 'read',
    'dint',
    'type',
    'not-implemented',
    'idle',
    'terminate',
    'push-lnil',
    'push-tnil',
    'reverse',
    'pack',
    'invoke',
    'add', 'sub', 'mul', 'div', 'pow',
    'and', 'or', 'xor', 'not', 'shl', 'shr', 'shll',
  ]

  unary_instructions = [
    'bind', 'fun-arg',
    'jump',
    'match',
    'hmatch',
    'rjump',
    'push-lit',
    'call', 'push-fun',
  ]

  binary_instructions = [
    'eval-and-push', 'push-name', 'closure',
    'eval-tail',
    'import-explicit', 'import-implicit',
  ]

  multiarity_instructions = [
  ]

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

  for i in ['fint', 'ufint', 'int', 'float', 'str']
    nullary_instructions.push "to-#{i}"
  end

  NUINST = nullary_instructions
  UNINST = unary_instructions
  BIINST = binary_instructions
  MAINST = multiarity_instructions
  INST_LABELS = inst_that_supports_label_as_arguments
end
