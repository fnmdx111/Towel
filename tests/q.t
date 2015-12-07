"---
Holistic test: Quicksort.
!>>
!<< [1 1 2 2 3 3 4 4 5 5 6 7 8 9 10]
"

import 'std' \

bind #quicksort ,\ L,
  (L ?#empty\Std ift (!!pop\Std []), (!!pop\Std
     L #tl\Std (L #hd\Std >\Std) /filter\Std #quicksort
     L #hd\Std [] #cons\Std #concat\Std
     L #tl\Std (L #hd\Std <=\Std) /filter\Std #quicksort #concat\Std))
then ([5 4 3 2 1 10 9 8 7 6 5 4 3 2 1] #quicksort !println\Std)

