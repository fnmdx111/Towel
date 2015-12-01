
import '.w' \

bind + fun` ~1 ~2, (~1 ~2 ..+\.w)
also - fun` ~1 ~2, (~1 ~2 ..-\.w)
also * fun` ~1 ~2, (~1 ~2 ..*\.w)
also / fun` ~1 ~2, (~1 ~2 ../\.w)
also ** fun` ~1 ~2, (~1 ~2 ..**\.w)
also ~fint fun` ~1, (~1 ..2fint\.w)
"Use tilde as prefix for all the conversion functions."
also ~ufint fun` ~1, (~1 ..2ufint\.w)
also ~int fun` ~1, (~1 ..2int\.w)
also ~float fun` ~1, (~1 ..2float\.w)
also ~str fun` ~1, (~1 ..2str\.w)
also !print .!print\.w`
"Use exclamation mark as prefix for functions with side-effects."
"Use .! as prefix for functions with serious stack-effects."
also !println .!println\.w`
also #hd fun` ~1, (~1 ..hd\.w)
"Use # as prefix for list/tuple functions."
also #tl fun` ~1, (~1 ..tl\.w)
also #cons fun` ~1 ~2, (~1 ~2 ..cons\.w)
also #?empty fun` ~1, (~1 .?empty\.w)
also #t1 fun` ~1, (~1 ..t1\.w)
also #t2 fun` ~1, (~1 ..t2\.w)
also #t3 fun` ~1, (~1 ..t3\.w)
then export + - * / ** ~fint ~ufint ~int ~float ~str !print !println
#hd #tl #cons #?empty #t1 #t2 #t3 @

