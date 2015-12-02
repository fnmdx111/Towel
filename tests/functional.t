"---
Tests some of the functional functions.
!>>
!<< [41 40]
[1 3 false]
"
import 'std' @

bind :dec-1 (1 -)
then ([42 41] :dec-1` :map !println)

bind ?not-true fun` ~x, (~x true = ift true, false)
then ([1 3 true false true] ?not-true` :filter !println)
