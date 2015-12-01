"Greatest common divisor test case.
---
Computes the greatest common divisor of 42 and 24.
Mainly tests tail recursive calls.
!>> 
!<< 6
"

import 'std' \
import '.w' \

bind GCD fun` A B,
  (A B -\Std if=0 A,
     if>0 (.!pop\.w A B -\Std B GCD@),
     if<0 (.!pop\.w A B A -\Std GCD@),
       idle)
then (42 24 GCD !println\Std)
