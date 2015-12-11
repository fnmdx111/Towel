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

import 'std' \

bind A fun` B,
  bind C fun` D,
    bind E (1 2 -\Std)
    then (((E !println\Std D !println\Std B !println\Std 10)))
  also F fun` D,
    bind E (5 7 -\Std)
    then (E !println\Std D !println\Std B !println\Std 12)
  then (B !println\Std 5 C !println\Std 9 F !println\Std 11)
then (3 A !println\Std)
