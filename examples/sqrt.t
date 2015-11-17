import 'std' 'math' @

bind EPS 0.0000001
also Sqrt fun X Q,
  (X **f Q -f Abs EPS -f
   if<0 X,
   if>0 (Pop X 1 + Q Sqrt@),
   X)
then (1 5 Sqrt
export Sqrt @).
  