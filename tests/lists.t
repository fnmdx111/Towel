"---
Tests various list actions.
!>>
!<< 1
2
3
2
[42]
"

import 'std' @

bind -my-list [1 2 3 [3 4] [42]]
then (
  -my-list .hd !println
  -my-list .tl .hd !println
  -my-list .tl .tl .tl .hd .hd "really a pain in the butt" !println
  -my-list .t2 !println
  -my-list .tl .tl .tl .tl .hd !println
)