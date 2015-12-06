"---
Tests if phony works.
!>>
!<< -1
"
import 'std' @
import '.w' \

(1 $$\.w 2 - .!invoke\.w !println)
