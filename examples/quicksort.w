"bind Quicksort fun L,"
"  match [], [];"
"        Head Tail ::,"
"          (Tail (Head <) Filter\Std Quicksort"
"           [Head]"
"           Tail (Head >=) Filter\Std Quicksort)"
"then ([5 4 3 2 1] Quicksort Print-list)."


                   push-scope
                     "pushing a new scope onto the scope stack, see scoping.ml"
                   push-fun :fv1
                   bind Quicksort
                     "bind TOS to the name Quicksort"
                   fun-arg L
                     "see also gcd.w"
:fv1-st            copy-from-parent-stack 1
                   push-scope
                   mchpush-list 0
                   match :fv1-m1
                     "vm is responsible for popping mchstack"
:fv1-m1            push-list 0
:fv1-end1          ret
                     "ret is responsible for cleaning up, for example, copying TOS to"
                     "parent stack, popping scope, popping data stack, etc."
:fv1-m1!           mchpush-name Head
                   mchpush-name Tail
                   mchpush-name ::
                   match :fv1-m2
:fv1-m2            push-seq :fv1-seq1
:fv1-seq1-st       push-scope
                   push-name Tail
                   push-seq :fv1-seq1-seq1
                     "it's written push-seq, but it's actually eval-seq at :fv1-seq1-seq1,"
                     "except when you are backquoting it"
:fv1-seq1-seq1-st  push-scope
                     "sequences might be called recursively, because they are also functions,"
                     "so it's vital that we also push scopes when evaluating them"
                   push-name Head
                   push-name <
:fv1-seq1-seq1-end ret-seq
                   push-name Filter Std
                   push-name Quicksort
                   push-name Head
                   push-list 1
                   push-name Tail
                   push-seq :fv1-seq1-seq2
:fv1-seq1-seq2-st  push-scope
                   push-name Head
                   push-name >=
:fv1-seq1-seq2-end ret-seq
                   push-name Filter Std
                   push-name Quicksort
:fv1-seq1-end      ret-seq
:fv1-m2! :fv1-end2 ret
                   push-seq :seq1
:seq1-st           push-scope
                   push-int 5
                   push-int 4
                   push-int 3
                   push-int 2
                   push-int 1
                   push-list 5
                   push-name Quicksort
                   push-name Print-list
                   ret-seq
:seq1-end          terminate
