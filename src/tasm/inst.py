"""
Definition for the instructions of Towel Assembly Language.

See also the Towel Assembly Language Manual.
"""

nullary_instructions = [
    'push-scope',
    'pop-scope',
    'make-backquote',
    'end-list',
    'end-tuple',
    'pop',
    'unpack',
    'push-stack',
    'share-stack',
    'pop-stack',
    'ret',
    'shared-ret',
    'show',
    'read',
    'dint',
    'type',
    'unimplemented',
    'idle',
    'backquote-name',
    'backquote',
    'patbackquote',
    'terminate',
    'not-implemented',
]

unary_instructions = [
    'bind',
    'fun-arg',
    'pack',
    'jump',
    'match',
    'hmatch',
    'rjump',
    'import',
]

multiarity_instructions = [
    'push-name',
    'patpush-name',
    'push-tail-name',
]

for i in ['make', 'push', 'patpush']:
    for j in ['list', 'tuple']:
        nullary_instructions.append('%s-%s' % (i, j))

    for j in ['fun', 'int', 'fint', 'ufint', 'float', 'string',
              'atom']:
        unary_instructions.append('%s-%s' % (i, j))

inst_that_supports_label_as_argument = {
    'jump', 'match', 'hmatch'
}

for i in ['make', 'push', 'patpush']:
    inst_that_supports_label_as_argument.add('%s-fun' % i)

branching_inst_that_doesnt_support_hungry_mode = {
    'e', 'ne'
}

for i in ['gez', 'gz', 'lez', 'lz', 'ez', 'nez', 't', 'f', 'e', 'ne']:
    unary_instructions.append('j%s' % i)
    inst_that_supports_label_as_argument.add('j%s' % i)

    if i not in branching_inst_that_doesnt_support_hungry_mode:
        unary_instructions.append('hj%s' % i)
        inst_that_supports_label_as_argument.add('hj%s' % i)

for i in ['fint', 'ufint', 'int', 'float']:
    for j in ['add', 'sub', 'mul', 'div', 'pow', 'equ']:
        nullary_instructions.append('%s-%s' % (i, j))

for i in ['fint', 'ufint']:
    for j in ['and', 'or', 'xor', 'not', 'shl', 'shr', 'shll']:
        nullary_instructions.append('%s-%s' % (i, j))
