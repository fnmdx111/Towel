"---
Tests some of the functional functions.
!>>
!<< [41 40]
"
import 'std' @

bind :dec-1 (1 -)
then ([42 41] :dec-1` :map !println)
