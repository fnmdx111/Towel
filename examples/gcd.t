bind GCD fun A B,
  (- if=0 import 'a' 'b' 'c' @,
     if>0 (A A B - GCD@),
     if<0 (B A - B GCD@), 0)
then (42 24 GCD
	export GCD @).
