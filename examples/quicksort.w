"bind Quicksort fun L,"
"  match [], [];"
"        Head Tail ::,"
"          (Tail (Head <) Filter\Std Quicksort"
"           [Head]"
"           Tail (Head >=) Filter\Std Quicksort)"
"then ([5 4 3 2 1] Quicksort Print-list)."


                   push-new-scope
                   bind Quicksort
                   make-fun :fv1
                   fun-arg L
:fv1-st            copy-from-parent-stack 1
                   push-new-scope
                   mchpush-lit-list
                   match :fv1-st-m1
                     "vm is responsible for popping mchstack"
:fv1-m1            push-lit-list 0
:fv1-end1          ret
:fv1-m1!           mchpush-name Head
                   mchpush-name Tail
                   mchpush-name ::
                   match :fv1-st-m2
:fv1-m2            pushmake-seq :fv1-seq1
:fv1-seq1-st       push-name Tail
                   pushmake-seq :fv1-seq1-seq1
:fv1-seq1-seq1-st  push-name Head
                   push-name <
:fv1-seq1-seq1-end push-name Filter Std
                   push-name Quicksort
                   push-name Head
                   push-lit-list 1
                   push-name Tail
                   pushmake-seq :fv1-seq1-seq2
:fv1-seq1-seq2-st  push-name Head
                   push-name >=
:fv1-seq1-seq2-end push-name Filter Std
                   push-name Quicksort
:fv1-seq1-end :fv1-m2! :fv1-end2 ret
                   make-seq :seq1
:seq1-st           push-lit-int 5
                   push-lit-int 4
                   push-lit-int 3
                   push-lit-int 2
                   push-lit-int 1
                   push-lit-list 5
                   push-name Quicksort
                   push-name Print-list
:seq1-end          terminate
