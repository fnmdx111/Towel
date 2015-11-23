push-scope
make-fun 4u
bind 1u "+"
jump 6u
fint-add
shared-ret
make-fun 9u
bind 2u "-"
jump 11u
fint-sub
shared-ret
make-fun 14u
bind 3u "*"
jump 16u
fint-mul
shared-ret
make-fun 19u
bind 4u "/"
jump 21u
fint-div
shared-ret
make-fun 24u
bind 5u "**"
jump 26u
fint-pow
shared-ret
make-fun 29u
bind 6u "="
jump 31u
fint-equ
shared-ret
make-fun 34u
bind 7u "+u"
jump 36u
ufint-add
shared-ret
make-fun 39u
bind 8u "-u"
jump 41u
ufint-sub
shared-ret
make-fun 44u
bind 9u "*u"
jump 46u
ufint-mul
shared-ret
make-fun 49u
bind 10u "/u"
jump 51u
ufint-div
shared-ret
make-fun 54u
bind 11u "**u"
jump 56u
ufint-pow
shared-ret
make-fun 59u
bind 12u "=u"
jump 61u
ufint-equ
shared-ret
make-fun 64u
bind 13u "+f"
jump 66u
float-add
shared-ret
make-fun 69u
bind 14u "-f"
jump 71u
float-sub
shared-ret
make-fun 74u
bind 15u "*f"
jump 76u
float-mul
shared-ret
make-fun 79u
bind 16u "/f"
jump 81u
float-div
shared-ret
make-fun 84u
bind 17u "**f"
jump 86u
float-pow
shared-ret
make-fun 89u
bind 18u "=f"
jump 91u
float-equ
shared-ret
make-fun 94u
bind 19u "+i"
jump 96u
int-add
shared-ret
make-fun 99u
bind 20u "-i"
jump 101u
int-sub
shared-ret
make-fun 104u
bind 21u "*i"
jump 106u
int-mul
shared-ret
make-fun 109u
bind 22u "/i"
jump 111u
int-div
shared-ret
make-fun 114u
bind 23u "**i"
jump 116u
int-pow
shared-ret
make-fun 119u
bind 24u "=i"
jump 121u
int-equ
shared-ret
make-fun 124u
bind 25u "!print"
jump 126u
show
shared-ret
make-fun 129u
bind 26u "!println"
jump 133u
show
push-lit '
'
show
shared-ret
make-fun 136u
bind 27u "!pop"
jump 138u
pop
shared-ret
terminate
