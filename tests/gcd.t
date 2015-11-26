"Greatest common divisor test case.
---
Computes the greatest common divisor of 42 and 24.
Mainly tests tail recursive calls.
!>> 
!<< 6
"

import 'std' @

bind GCD fun` A B,
  (- if=0 A,
     if>0 (!pop A A B - GCD@),
     if<0 (!pop B A - B GCD@),
       idle)
then (42 24 GCD !println).
