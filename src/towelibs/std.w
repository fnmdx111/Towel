push-scope
push-stack
push-fun 5u
bind 1u "..+"
jump 7u
add
shared-ret
push-fun 10u
bind 2u "..-"
jump 12u
sub
shared-ret
push-fun 15u
bind 3u "..*"
jump 17u
mul
shared-ret
push-fun 20u
bind 4u "../"
jump 22u
div
shared-ret
push-fun 25u
bind 5u "..**"
jump 27u
pow
shared-ret
push-fun 30u
bind 6u "..2fint"
jump 32u
to-fint
shared-ret
push-fun 35u
bind 7u "..2ufint"
jump 37u
to-ufint
shared-ret
push-fun 40u
bind 8u "..2int"
jump 42u
to-int
shared-ret
push-fun 45u
bind 9u "..2float"
jump 47u
to-float
shared-ret
push-fun 50u
bind 10u "..2str"
jump 52u
to-str
shared-ret
push-fun 55u
bind 11u ".!print"
jump 57u
show
shared-ret
push-fun 60u
bind 12u ".!println"
jump 64u
show
push-lit '
'
show
shared-ret
push-fun 67u
bind 13u ".!pop"
jump 69u
pop
shared-ret
push-fun 72u
bind 14u ".!rev"
jump 74u
reverse
shared-ret
push-fun 77u
bind 15u ".!probe"
jump 79u
dint
shared-ret
push-fun 82u
bind 16u ".!invoke"
jump 84u
invoke
shared-ret
push-fun 87u
bind 17u "..hd"
jump 89u
car
shared-ret
push-fun 92u
bind 18u "..tl"
jump 94u
cdr
shared-ret
push-fun 97u
bind 19u "..cons"
jump 99u
cons
shared-ret
push-fun 102u
bind 20u ".?empty"
jump 104u
list-empty
shared-ret
push-fun 107u
bind 21u "..t1"
jump 109u
car
shared-ret
push-fun 112u
bind 22u "..t2"
jump 115u
cdr
car
shared-ret
push-fun 118u
bind 23u "..t3"
jump 122u
cdr
cdr
car
shared-ret
terminate
