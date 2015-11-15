 import 'std' 1u
 make-fun 4u
 bind 1u
 jump 31u
 push-stack
 push-scope
 fun-arg 2u
 fun-arg 3u
 push-name 3u 0u
 push-name 2u 0u
 push-name 2u 1u
 jnez 14u
 push-name 2u 0u
 jump 29u
 jlez 21u
 push-name 2u 0u
 push-name 2u 0u
 push-name 3u 0u
 push-name 2u 1u
 push-tail-name 1u 0u
 jump 29u
 jgez 28u
 push-name 3u 0u
 push-name 2u 0u
 push-name 2u 1u
 push-name 3u 0u
 push-tail-name 1u 0u
 jump 29u
 push-fint 0
 pop-scope
 ret
 push-fint 42
 push-fint 24
 push-name 1u 0u
 push-name 0u 0u
 terminate