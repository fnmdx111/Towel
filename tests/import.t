"---
Tests implicit and explicit import. Also tests if namespace works.
!>>
!<< Hello world!
Hello world!
"

import 'std' @
('Hello world!' .!println)

import 'std' \
('Hello world!' .!println\Std)