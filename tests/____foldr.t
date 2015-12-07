
import 'std' @
import '.w' \

bind *foldr ,\ ~l ~p ~f,
  (~l ?# ift (.!pop\.w ~p),
   (.!pop\.w bind ~x (~l ..hd\.w)
             then (~p ~l ..tl\.w ~f` *foldr
                   ~x
                   ~f)))
then ([] [1 2 3 4 5 6 7 8] fun` ~acc ~x, (0 ~x - ~acc #cons) /foldr !println)