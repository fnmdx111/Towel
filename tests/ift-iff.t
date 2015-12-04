"---
Tests ift and iff instruction. And other ifs.
!>> 
!<< 12211221"

import 'std' \
import '.w' @

(@@ ife (1 !print\Std), (2 !print\Std))
(@@ ifne (1 !print\Std), (2 !print\Std))
(1 2 3 ife (1 !print\Std), (2 !print\Std))
(1 2 3 ifne (1 !print\Std), (2 !print\Std))

(true ift (1 !print\Std), (2 !print\Std)) .!pop\.w
(false ift (1 !print\Std), (2 !print\Std)) .!pop\.w
(true iff (1 !print\Std), (2 !print\Std)) .!pop\.w
(false iff (1 !print\Std), (2 !print\Std)) .!pop\.w
