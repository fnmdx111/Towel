"---
Tests partial function application.
!>>
!<< 42
"

import 'std' @

bind Dec1 (1 fun A B, (A B -))
then (43 Dec1 !println).