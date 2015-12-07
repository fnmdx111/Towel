"---
Holistic test: Quicksort.
!>>
!<< [1 2 3 4 5]
"

import 'std' @

bind #quicksort ,\ L,
  (L ?#empty ift (!!pop []), (!!pop
    bind ~h (L #hd)
    also ~t (L #tl)
    then (~t (~h >) /filter #quicksort
          ~h [] #cons
          ~t (~h <=) /filter #quicksort
      #concat #concat)))
then ([5 4 3 2 1] #quicksort !println)

