"---
Tests partial function application.
!>>
!<< 42
41
"

import '.w' @

bind Dec1 (1 fun . /, (. / ..-))
then (43 Dec1 .!println 42 Dec1 .!println)