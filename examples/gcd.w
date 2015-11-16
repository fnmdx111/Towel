 import-implicit 'std' 1u
 make-fun 4u
 bind 4u
 jump 31u
 push-stack
 push-scope
 fun-arg 5u
 fun-arg 6u
 push-name 6u 0u
 push-name 5u 0u
 push-name 3u 0u
 jnez 14u
 push-name 5u 0u
 jump 29u
 jlez 21u
 push-name 5u 0u
 push-name 5u 0u
 push-name 6u 0u
 push-name 3u 0u
 push-tail-name 4u 0u
 jump 29u
 jgez 28u
 push-name 6u 0u
 push-name 5u 0u
 push-name 3u 0u
 push-name 6u 0u
 push-tail-name 4u 0u
 jump 29u
 push-fint 0
 pop-scope
 ret
 push-fint 42
 push-fint 24
 push-name 4u 0u
 push-name 0u 0u
 terminate