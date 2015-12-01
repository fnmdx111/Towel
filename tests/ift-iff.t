"---
Tests ift and iff instruction.
!>> 
!<< 1221"

import 'std' @

(true ift (1 !print), (2 !print))
(false ift (1 !print), (2 !print))
(true iff (1 !print), (2 !print))
(false iff (1 !print), (2 !print))