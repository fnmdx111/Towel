"---
Tests if nested bind and also works.
!>>
!<< **fun: 108,1,<closure set>
**fun: 172,1,<closure set>
3
**fun: 108,1,<closure set>
-1
5
**fun: 172,1,<closure set>
3
**fun: 108,1,<closure set>
10
-2
9
**fun: 172,1,<closure set>
3
**fun: 108,1,<closure set>
**fun: 144,1,<closure set>
12
11
"

import 'std' @

bind A fun` B,
  bind C fun` D,
    bind E (1 2 -)
    then (((E !println D !println C` !println B !println A` !println 10)))
  also F fun` D,
    bind E (5 7 -)
    then (E !println D !println C` !println B !println A` !println F` !println 12)
  then (C` !println B !println A` !println 5 C !println 9 F !println 11)
then (A` !println 3 A !println)
