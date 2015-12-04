"---
Tests some of the stack instructions.
!>> 
!<< 42
0
1
"
import '.w' \
import 'std' \

(42 24 .!pop\.w !println\Std)

(21 .!dup\.w -\Std !println\Std)

(21 20 2 .!pack\.w -\Std` /apply\Std !println\Std)