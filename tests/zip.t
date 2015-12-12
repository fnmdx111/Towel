"---
Tests advanced enumerable literal creation.
!>>
!<< [[@ 1 2] [@ 2 3] [@ 3 4] [@ 4 5] [@ 5 6]]
"

import 'std' @

bind #zip fun` ~1 ~2,
  bind ~f ,\ ~acc ~xs1 ~xs2,
    (~xs1 ?# ift ~acc, (
     ~xs2 ?# ift ~acc, (
       [\(~xs1 #hd) (~xs2 #hd)] ~acc #cons
       ~xs1 #tl ~xs2 #tl ~f@)))
  then ([] ~1 ~2 ~f #rev@)
then ([1 2 3 4 5] [2 3 4 5 6] #zip !println)
