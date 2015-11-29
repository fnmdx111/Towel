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
bind 6u "="
jump 32u
fint-equ
shared-ret
make-fun 35u
bind 7u "+u"
jump 37u
ufint-add
shared-ret
make-fun 40u
bind 8u "-u"
jump 42u
ufint-sub
shared-ret
make-fun 45u
bind 9u "*u"
jump 47u
ufint-mul
shared-ret
make-fun 50u
bind 10u "/u"
jump 52u
ufint-div
shared-ret
make-fun 55u
bind 11u "**u"
jump 57u
ufint-pow
shared-ret
make-fun 60u
bind 12u "=u"
jump 62u
ufint-equ
shared-ret
make-fun 65u
bind 13u "+f"
jump 67u
float-add
shared-ret
make-fun 70u
bind 14u "-f"
jump 72u
float-sub
shared-ret
make-fun 75u
bind 15u "*f"
jump 77u
float-mul
shared-ret
make-fun 80u
bind 16u "/f"
jump 82u
float-div
shared-ret
make-fun 85u
bind 17u "**f"
jump 87u
float-pow
shared-ret
make-fun 90u
bind 18u "=f"
jump 92u
float-equ
shared-ret
make-fun 95u
bind 19u "+i"
jump 97u
int-add
shared-ret
make-fun 100u
bind 20u "-i"
jump 102u
int-sub
shared-ret
make-fun 105u
bind 21u "*i"
jump 107u
int-mul
shared-ret
make-fun 110u
bind 22u "/i"
jump 112u
int-div
shared-ret
make-fun 115u
bind 23u "**i"
jump 117u
int-pow
shared-ret
make-fun 120u
bind 24u "=i"
jump 122u
int-equ
shared-ret
make-fun 125u
bind 25u "!print"
jump 127u
show
shared-ret
make-fun 130u
bind 26u "!println"
jump 134u
show
push-lit '
'
show
shared-ret
make-fun 137u
bind 27u "!pop"
jump 139u
pop
shared-ret
make-fun 142u
bind 28u "!rev"
jump 144u
reverse
shared-ret
terminate
