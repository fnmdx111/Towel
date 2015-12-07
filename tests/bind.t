"---
Tests if nested bind and also works.
!>>
!<< 3
-1
5
3
10
-2
9
3
12
11
"

import 'std' @

bind A fun` B,
  bind C fun` D,
    bind E (1 2 -)
    then (((E !println D !println B !println 10)))
  also F fun` D,
    bind E (5 7 -)
    then (E !println D !println B !println 12)
  then (B !println 5 C !println 9 F !println 11)
then (3 A !println)
