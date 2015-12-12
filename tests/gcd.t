"Greatest common divisor test case.
---
Computes the greatest common divisor of 42 and 24.
Mainly tests tail recursive calls.
!>> 
!<< 6
"

import 'std' @

bind GCD fun` A B,
  (A B - if=0 A,
     if>0 (A B - B GCD@),
     if<0 (A B A - GCD@),
       ~idle)
then (42 24 GCD !println)
