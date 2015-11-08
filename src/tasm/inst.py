
nullary_instructions = [
    'push-scope',
    'pop-scope',
    'make-list',
    'make-tuple',
    'end-list',
    'end-tuple',
    'pop',
    'unpack',
    'push-stack',
    'share-stack',
    'pop-stack',
    'ret',
    'show',
    'read',
    'dint',
    'unimplemented',
    'idle',
    'backquote-name',
    'terminate',
]

unary_instructions = [
    'bind',
    'fun-arg',
    'pack',
    'jump',
    'match',
    'hmatch',
    'backquote',
    'patbackquote',
    'make-backquote',
]

multiarity_instructions = [
    'push-name',
    'push-tail-name',
]

for i in ['make', 'push', 'patpush']:
    for j in ['fun', 'int', 'fint', 'ufint', 'float', 'string',
              'atom']:
        unary_instructions.append('%s-%s' % (i, j))

inst_that_supports_label_as_argument = {
    'jump', 'match', 'hmatch'
}
for i in ['gez', 'gz', 'lez', 'lz', 'ez', 'nez', 't', 'f', 'e', 'ne']:
    unary_instructions.append('j%s' % i)
    unary_instructions.append('hj%s' % i)
    inst_that_supports_label_as_argument.add('j%s' % i)
    inst_that_supports_label_as_argument.add('hj%s' % i)

for i in ['fint', 'ufint', 'int', 'float']:
    for j in ['add', 'sub', 'mul', 'div', 'pow', 'equ']:
        nullary_instructions.append('%s-%s' % (i, j))

for i in ['fint', 'ufint']:
    for j in ['and', 'or', 'xor', 'not', 'shl', 'shr', 'shll']:
        nullary_instructions.append('%s-%s' % (i, j))
