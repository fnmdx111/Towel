"---
Holistic test: Quicksort.
!>>
!<< [1 2 3 4 5]
"

import '.w' \
import 'std' \

bind #quicksort ,\ L,
  ('L is ' !println\Std L !println\Std L ?#empty\Std ift (
  .!pop\.w
  'empty' !println\Std [] 'return' !println\Std ), (.!pop\.w
    bind ~h (L #hd\Std)
    also ~t (L #tl\Std)
    then (~h !println\Std ~t !println\Std
      ~t $$\.w ~h >\Std /filter\Std
        'bigger are ' !println\Std ~t $$\.w ~h >\Std /filter\Std !println\Std #quicksort
      ~h [] #cons\Std 'pivot is' !println\Std ~h [] #cons\Std !println\Std
      ~t $$\.w ~h <=\Std /filter\Std
        'less are ' !println\Std ~t $$\.w ~h <=\Std /filter\Std !println\Std #quicksort
      #concat\Std #concat\Std)))
then ([5 4 3 2 1] #quicksort !println\Std)

