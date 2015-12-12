"---
Some object-oriented programming sh**.
!>>
!<< Circle
Rectangle
5541.76476
2
"

import 'std' @

bind >>send !invoke`

also Shape ,\ ~type,
  bind __type ~type
  also Type fun`, #1
  also Area #2`
  then [\__type [\Type` Area`]]

then bind __meta-Shape ('' Shape)
     also :type (__meta-Shape #2 #1)
     also :area (__meta-Shape #2 #2)

     also Circle ,\ Radius,
       bind __radius Radius
       also ~super ('Circle' Shape)

       then bind Type (~super #1)
            also Area ,\, (__radius __radius 3.14159 * *)

            then [\Type` Area`]

      also Rectangle ,\ Width Height,
        bind __width Width
        also __height Height

        also ~super ('Rectangle' Shape)

        then bind Type (~super #1)
             also Area ,\, (__width __height *)

             then [\Type` Area`]

      then bind ~my-circle (42. Circle)
           also ~my-rectangle (1 2 Rectangle)
	   then (~my-circle :type >>send !println
	         ~my-rectangle :type >>send !println
		 ~my-circle :area >>send !println
		 ~my-rectangle :area >>send !println)

