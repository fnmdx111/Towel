"---
Tests various list actions.
!>>
!<< 1
2
3
[42]
[3 1 2]
[1 2]
[1 2 3 4 5]
3
"

import 'std' @

bind -my-list [1 2 3 [3 4] [42]]
then (
  -my-list #hd !println
  -my-list #tl #hd !println
  -my-list #tl #tl #tl #hd #hd "really a pain in the butt" !println
  -my-list #tl #tl #tl #tl #hd !println
)

bind -my-list [1 2]
then (
  "tests if lists are immutable"
  3 -my-list #cons !println
  -my-list !println
  -my-list [3 4 5] |#| !println
)

bind ~my-tuple [\ 1 2 [\ 3 4] [5 6] 7]
then (~my-tuple #t3 #t1 !println)