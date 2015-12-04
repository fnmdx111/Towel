
import '.w' \

bind !>> !>>\.w`

also ^~ ('ext_random.cmo' !>ext\.w)
"The ext module handle."

also ~seed fun` ~s, (~s 2u ^~ !>>)
also ~useed fun`, (1u ^~ !>>)
also ~~ fun`, (3u ^~ !>>)
then export ~seed ~useed ~~ @
