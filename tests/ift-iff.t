"---
Tests ift and iff instruction.
!>> 
!<< 1221"

import 'std' \

(true ift (1 !print\Std), (2 !print\Std))
(false ift (1 !print\Std), (2 !print\Std))
(true iff (1 !print\Std), (2 !print\Std))
(false iff (1 !print\Std), (2 !print\Std))