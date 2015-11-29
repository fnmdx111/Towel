push-scope
push-stack
make-fun 5u
bind 1u "+"
jump 7u
fint-add
shared-ret
make-fun 10u
bind 2u "-"
jump 12u
fint-sub
shared-ret
make-fun 15u
bind 3u "*"
jump 17u
fint-mul
shared-ret
make-fun 20u
bind 4u "/"
jump 22u
fint-div
shared-ret
make-fun 25u
bind 5u "**"
jump 27u
fint-pow
shared-ret
make-fun 30u
bind 6u "+u"
jump 32u
ufint-add
shared-ret
make-fun 35u
bind 7u "-u"
jump 37u
ufint-sub
shared-ret
make-fun 40u
bind 8u "*u"
jump 42u
ufint-mul
shared-ret
make-fun 45u
bind 9u "/u"
jump 47u
ufint-div
shared-ret
make-fun 50u
bind 10u "**u"
jump 52u
ufint-pow
shared-ret
make-fun 55u
bind 11u "+f"
jump 57u
float-add
shared-ret
make-fun 60u
bind 12u "-f"
jump 62u
float-sub
shared-ret
make-fun 65u
bind 13u "*f"
jump 67u
float-mul
shared-ret
make-fun 70u
bind 14u "/f"
jump 72u
float-div
shared-ret
make-fun 75u
bind 15u "**f"
jump 77u
float-pow
shared-ret
make-fun 80u
bind 16u "+i"
jump 82u
int-add
shared-ret
make-fun 85u
bind 17u "-i"
jump 87u
int-sub
shared-ret
make-fun 90u
bind 18u "*i"
jump 92u
int-mul
shared-ret
make-fun 95u
bind 19u "/i"
jump 97u
int-div
shared-ret
make-fun 100u
bind 20u "**i"
jump 102u
int-pow
shared-ret
make-fun 105u
bind 21u "!print"
jump 107u
show
shared-ret
make-fun 110u
bind 22u "!println"
jump 114u
show
push-lit '
'
show
shared-ret
make-fun 117u
bind 23u "!pop"
jump 119u
pop
shared-ret
make-fun 122u
bind 24u "!rev"
jump 124u
reverse
shared-ret
terminate
