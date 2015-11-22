import 'std' 'math' @

bind EPS 0.0000001
also Sqrt fun X Q,
  (X X *f Q -f Abs EPS -f
   if<0 X,
   if>0 (!pop X 1. +f Q Sqrt@),
   X)
then export Sqrt @.
