"---
Tests variadic functions.
!>>
!<< [1 2 3 4 5 4 5 6 7 8 2 3 4 5 6]
15
"

import 'std' @

$$ [1 2 3 4 5] [4 5 6 7 8] [2 3 4 5 6] #!vconcat !println

+` 0 $$ 1 2 3 4 5 /!vfoldl !println
