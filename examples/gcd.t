"Greatest common divisor test case.

---

Mainly tests tail recursive calls.
!>> 
!<< 6
"

import 'std' @

bind GCD fun A B,
  (- if=0 A,
     if>0 "asdfsfasdfasdfasdf
     sdfasdfasdfd" (A A B - GCD@),
     if<0 (B A - B GCD@), 0)
then (42 24 GCD Println
export GCD @).
