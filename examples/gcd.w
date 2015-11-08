 push-stack
 push-scope
 make-fun 5
 bind 0
 jump 29
 push-stack
 push-scope
 fun-arg 1
 fun-arg 2
 push-name -1l
 jez 13
 push-name 1l
 jump 28
 jgz 20
 push-name 1l
 push-name 1l
 push-name 2l
 push-name -1l
 push-tail-name 0l
 jump 28
 jlz 27
 push-name 2l
 push-name 1l
 push-name -1l
 push-name 2l
 push-tail-name 0l
 jump 28
 push-fint 0
 ret
 push-fint 42
 push-fint 24
 push-name 0l
 terminate