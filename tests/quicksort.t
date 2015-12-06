"---
Holistic test: Quicksort.
!>>
!<< [1 2 3 4 5]
"

import '.w' \
import 'std' \

bind #quicksort ,\ L,
  (L ?#empty\Std ift (.!pop\.w []), (.!pop\.w
     L #tl\Std $$\.w L #hd\Std >\Std /filter\Std #quicksort
     L #hd\Std [] #cons\Std
     L #tl\Std $$\.w L #hd\Std <=\Std /filter\Std #quicksort
     #concat\Std #concat\Std))
then ([5 4 3 2 1] #quicksort !println\Std)

