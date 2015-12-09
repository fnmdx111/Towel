"---
Tests partial function application.
!>>
!<< 42
41
[2 2]
"

import 'std' @

bind Dec1 (1 -)
then (43 Dec1 !println 42 Dec1 !println)

bind X 2
then ([X X] !println)