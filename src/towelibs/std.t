
import '.w' \

bind + fun` ~1 ~2, (~1 ~2 ..+\.w)
also - fun` ~1 ~2, (~1 ~2 ..-\.w)
also * fun` ~1 ~2, (~1 ~2 ..*\.w)
also / fun` ~1 ~2, (~1 ~2 ../\.w)
also ** fun` ~1 ~2, (~1 ~2 ..**\.w)
also % fun` ~1 ~2, (~1 ~2 ..%\.w)
also = fun` ~1 ~2, (~1 ~2 ..=\.w)

also > fun` ~1 ~2, (~1 ~2 - if>0 true, false)
also >= fun` ~1 ~2, (~1 ~2 - if>=0 true, false)
also < fun` ~1 ~2, (~1 ~2 - if<0 true, false)
also <= fun` ~1 ~2, (~1 ~2 - if<=0 true, false)
also <> fun` ~1 ~2, (~1 ~2 - if=0 false, true)

also :and fun` ~1 ~2, (~1 ~2 ..and\.w)
also :or fun` ~1 ~2, (~1 ~2 ..or\.w)
also :not fun` ~1 ~2, (~1 ~2 ..not\.w)

also ~fint fun` ~1, (~1 ..2fint\.w)
"Use tilde as prefix for all the conversion functions."
also ~ufint fun` ~1, (~1 ..2ufint\.w)
also ~int fun` ~1, (~1 ..2int\.w)
also ~float fun` ~1, (~1 ..2float\.w)
also ~str fun` ~1, (~1 ..2str\.w)

also ^? .^?\.w`

also !print .!print\.w`
"Use exclamation mark as prefix for functions with side-effects."
"Use .! as prefix for functions with serious stack-effects."
also !println .!println\.w`
also !read .!read\.w`

also !!pop .!pop\.w`
also !!dup .!dup\.w`

also $$ $$\.w`

also #hd fun` ~1, (~1 ..hd\.w)
also <# #hd`
"Use # as prefix for list/tuple functions."
also #tl fun` ~1, (~1 ..tl\.w)
also #> #tl`
also #cons fun` ~1 ~2, (~1 ~2 ..cons\.w)
also <#> #cons`
also ?#empty fun` ~1, (~1 .?empty\.w)
also ?# ?#empty`
also #tn fun` ~n ~t, (~t ~n ..tuple-at\.w)
also #t1 fun` ~1, (0 ~1 #tn)
also #t2 fun` ~1, (1 ~1 #tn)
also #t3 fun` ~1, (2 ~1 #tn)

also !!pack .!pack\.w`
also !!unpack .!unpack\.w`

also /id fun` ~1, ~1
"/\ -> lambda, since \ is reserved, we use /"
also /foldl fun` ~p ~l ~f, (
  ~l .?empty\.w ift (.!pop\.w ~p),
  (.!pop\.w ~p ~l ..hd\.w ~f "accumulated value"
   ~l ..tl\.w "rest of the list"
   ~f`
   /foldl@))
also /foldr fun` ~p ~l ~f, (
  ~l .?empty\.w ift (.!pop\.w ~p),
  (.!pop\.w ~p ~l ..tl\.w ~f` /foldr
   ~l ..hd\.w
   ~f))
also #rev fun` ~1, ([] ~1 fun` ~acc ~x, (~x ~acc ..cons\.w) /foldl@)
also <<# #rev`
also |#| fun` ~1 ~2, (~2 ~1 #rev fun` ~acc ~x, (~x ~acc ..cons\.w) /foldl@)
also #concat |#|`
also /map fun` ~l ~f, (
  [] ~l fun` ~acc ~x, (~x ~f ~acc ..cons\.w) /foldl #rev@)
also /filter fun` ~l ?pred, (
  [] ~l
  fun` ~acc ~x, (~x ?pred ift (.!pop\.w ~acc),
                 (.!pop\.w ~x ~acc ..cons\.w))
  /foldl #rev@)
also /flip fun` ~1 ~2 ~f, (~2 ~1 ~f)
also #len fun` ~1, (0 ~1 fun` ~acc _, (~acc 1 ..+\.w) /foldl@)
also !# #len`

also /apply fun` ~args ~f, (~args .!unpack\.w ~f)

then export + - * / ** % = :and :or ~fint ~ufint ~int ~float ~str !print
!println #hd #tl #cons ?#empty #t1 #t2 #t3 /id /foldl #rev /map /filter
/flip /foldr |#| #concat <# #> <#> ?# <<# !# /apply !read > >= < <= <> :not
!!pop !!dup !!pack !!unpack $$ ^? #tn @

