 push-stack
 push-scope
 make-fun 5
 bind 0
 jump 32
 push-stack
 push-scope
 fun-arg 1
 fun-arg 2
 push-name 2
 push-name 1
 push-name -1
 jnez 15
 push-name 1
 jump 30
 jlez 22
 push-name 1
 push-name 1
 push-name 2
 push-name -1
 push-tail-name 0
 jump 30
 jgez 29
 push-name 2
 push-name 1
 push-name -1
 push-name 2
 push-tail-name 0
 jump 30
 push-fint 0
 pop-scope
 ret
 push-fint 42
 push-fint 24
 push-name 0
 terminate