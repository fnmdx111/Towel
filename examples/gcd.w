 make-fun 3
 bind 1u
 jump 30
 push-stack
 push-scope
 fun-arg 2u
 fun-arg 3u
 push-name 3u
 push-name 2u
 push-name 0u
 jnez 13
 push-name 2u
 jump 28
 jlez 20
 push-name 2u
 push-name 2u
 push-name 3u
 push-name 0u
 push-tail-name 1u
 jump 28
 jgez 27
 push-name 3u
 push-name 2u
 push-name 0u
 push-name 3u
 push-tail-name 1u
 jump 28
 push-fint 0
 pop-scope
 ret
 push-fint 42
 push-fint 24
 push-name 1u
 terminate