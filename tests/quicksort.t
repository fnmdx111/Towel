"---
Holistic test: Quicksort.
!>>
!<< [1 2 3 4 5]
"

import '.w' \
import 'std' \

bind #quicksort ,\ L,
  (L ?#empty\Std ift (.!pop\.w []), (.!pop\.w
    bind ~h (L #hd\Std)
    also ~t (L #tl\Std)
    then (~t (~h >\Std) /filter\Std #quicksort
          ~h [] #cons\Std
          ~t (~h <=\Std) /filter\Std #quicksort
      #concat\Std #concat\Std)))
then ([5 4 3 2 1] #quicksort !println\Std)

