 make-fun 3u
 bind 1u
 jump 32u
 push-stack
 push-scope
 fun-arg 2u
 fun-arg 3u
 push-name 3u
 push-name 2u
 push-name 0u
 jnez 15u
 import 'a'
 import 'b'
 import 'c'
 jump 30u
 jlez 22u
 push-name 2u
 push-name 2u
 push-name 3u
 push-name 0u
 push-tail-name 1u
 jump 30u
 jgez 29u
 push-name 3u
 push-name 2u
 push-name 0u
 push-name 3u
 push-tail-name 1u
 jump 30u
 push-fint 0
 pop-scope
 ret
 push-fint 42
 push-fint 24
 push-name 1u
 terminate