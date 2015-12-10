"---
Tests if macros (simulated) works.
!>>
!<< -1
"

import 'std' \

bind Macro {if>0 -\Std, +\Std}
then (1 2 Macro !println\Std)
