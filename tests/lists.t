"---
Tests various list actions.
!>>
!<< 1
2
3
2
[42]
[3 1 2]
[1 2]
"

import '.w' @

bind -my-list [1 2 3 [3 4] [42]]
then (
  -my-list ..hd .!println
  -my-list ..tl ..hd .!println
  -my-list ..tl ..tl ..tl ..hd ..hd "really a pain in the butt" .!println
  -my-list ..t2 .!println
  -my-list ..tl ..tl ..tl ..tl ..hd .!println
)

bind -my-list [1 2]
then (
  "tests if lists are immutable"
  3 -my-list ..cons .!println
  -my-list .!println
)
