push-scope
push-stack
make-fun 5u
bind 1u "+"
jump 7u
add
shared-ret
make-fun 10u
bind 2u "-"
jump 12u
sub
shared-ret
make-fun 15u
bind 3u "*"
jump 17u
mul
shared-ret
make-fun 20u
bind 4u "/"
jump 22u
div
shared-ret
make-fun 25u
bind 5u "**"
jump 27u
pow
shared-ret
make-fun 30u
bind 6u "To-fint"
jump 32u
to-fint
shared-ret
make-fun 35u
bind 7u "To-ufint"
jump 37u
to-ufint
shared-ret
make-fun 40u
bind 8u "To-int"
jump 42u
to-int
shared-ret
make-fun 45u
bind 9u "To-float"
jump 47u
to-float
shared-ret
make-fun 50u
bind 10u "To-str"
jump 52u
to-str
shared-ret
make-fun 55u
bind 11u "!print"
jump 57u
show
shared-ret
make-fun 60u
bind 12u "!println"
jump 64u
show
push-lit '
'
show
shared-ret
make-fun 67u
bind 13u "!pop"
jump 69u
pop
shared-ret
make-fun 72u
bind 14u "!rev"
jump 74u
reverse
shared-ret
make-fun 77u
bind 15u "!probe"
jump 79u
dint
shared-ret
terminate
