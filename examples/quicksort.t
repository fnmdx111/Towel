bind Quicksort fun L,
  match [], [];
  Head Tail ::,
    (Tail (Head <)` Filter Quicksort
     [Head]
     Tail (Head >=)` Filter Quicksort
     ++ ++)
then ([5 4 3 2 1] Quicksort).

export Quicksort.
