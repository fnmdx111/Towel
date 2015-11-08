"bind Greatest-common-divisor fun X(Int) Y,"
"  (@ - if=0 X,"
"       if>0 (X Y - Y Greatest-common-divisor@),"
"            (X Y X - Greatest-common-divisor@))"
"then (42 24 Greatest-common-divisor)."

push-stack
push-scope
make-fun :1c93d6e6c-0-st
bind 0
jump :1c93d6e6c-0-real-end
:1c93d6e6c-0-st push-stack
push-scope
fun-arg 1
fun-arg 2
push-name -1L
jez :1c93d6e6c-1!
:1c93d6e6c-1 push-string 'asdfasdfasdf'
jump :1c93d6e6c-1-end
:1c93d6e6c-1! jgz :1c93d6e6c-2!
:1c93d6e6c-2 push-name 1L
push-name 1L
push-name 2L
push-name -1L
push-tail-name 0L
jump :1c93d6e6c-2-end
:1c93d6e6c-2! jlz :1c93d6e6c-3!
:1c93d6e6c-3 push-name 2L
push-name 1L
push-name -1L
push-name 2L
push-tail-name 0L
jump :1c93d6e6c-3-end
:1c93d6e6c-3! push-fint 0
:1c93d6e6c-3-end
:1c93d6e6c-2-end
:1c93d6e6c-1-end
:1c93d6e6c-0-end ret
:1c93d6e6c-0-real-end
push-fint 42
push-fint 24
push-name 0L
terminate
