"---
Tests some of the functional functions.
!>>
!<< 2
[41 40]
-123
[1 3 false]
-1
"
import 'std' @

bind :neg- (-` /flip)
then (1 3 :neg- !println)

bind :dec-1 (1 -)
then ([42 41] :dec-1` /map !println
      0 [42 41 40] -` /foldr !println)

bind ?not-true ,\ ~x, (~x ift true, false)
then ([1 3 true false true] ?not-true` /filter !println)

([1 2] -` /apply !println)
