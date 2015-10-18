bind Quicksort fun L,
  match
  [], [];
  Head Tail ::, (
    Tail (Head <)` Filter Quicksort
    [Head]
    Tail (Head >=)` Filter Quicksort
    ++ ++)
then (0 10 Range Quicksort Print-list).


type List [@ a List{a}] cons, nil
also BTree [@ BTree{a} BTree{a}] tree, [@ a] leaf.

type Option [@ a] some none
then bind A [@ 4] some\Option
then (A match X some\Option, (X println);
              none, (0 println)).
