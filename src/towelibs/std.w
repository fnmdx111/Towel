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
bind 5u "Print"
jump 26u
show
shared-ret
make-fun 29u
bind 6u "Println"
jump 33u
show
push-string '
'
show
shared-ret
make-fun 36u
bind 7u "Pop"
jump 38u
pop
shared-ret
terminate