"---
Tests if built-in routines work.
!>>
!<< 0.473146049408
0.717511397385
0.11174899528
"

import 'random' @
import '.w' \

(2u ~seed
 ~~ .!println\.w
 ~~ .!println\.w
 3u ~seed
 ~~ .!println\.w)
