"bind Quicksort fun L,"
"  match [], [];"
"        Head Tail ::,"
"          (Tail (Head <) Filter\Std Quicksort"
"           [Head]"
"           Tail (Head >=) Filter\Std Quicksort)"
"then ([5 4 3 2 1] Quicksort Print-list)."


                   push-scope
                     "pushing a new scope onto the scope stack, see scoping.ml"
		   push-stack
                   bind Quicksort
                     “bind Quicksort to the value created by next instruction"
                   make-fun :fv1
                     "because we are binding a name to a value here"
                     "so we use make-* rather than push-*"
                     "TVM creates this function value in the object table"
                     "after this instruction, IP jumps to the instruction right"
                     "after :fv1-end"
:fv1-st            push-scope
                     "when a function is called, IP points to here"
                   push-stack
                   fun-arg L
                     "pop the TOS of parent stack and bind it to name L,"
                     "see also gcd.w"
                   push-name L
                   mchpush-list 0
                     "push a empty list in the mchstack for pattern matching"
                   match :fv1-m1-p1
                     "if matching suceeds, jump to :fv1-m1-p1 otherwise :fv1-m1-p1!"
                     "vm is responsible for popping mchstack"
:fv1-m1-p1         make-list
                   push-list 0
                   mchend
                     "jump to the end of this match form"
:fv1-m1-p1!        mchpush-name Head
                   mchpush-name Tail
                   mchpush-name ::
                   match :fv1-m1-p2
:fv1-m1-p2         push-seq :fv1-seq1
                     "evaluate a nullary anonymous function whose body"
                     "starts at :fv1-seq1-st"
:fv1-seq1-st       push-scope
                   push-stack
                   push-name Tail
                   push-seq :fv1-seq1-seq1
                     "it's written push-seq, but it's actually eval-seq at :fv1-seq1-seq1,"
                     "except when you are backquoting it"
:fv1-seq1-seq1-st  push-scope
                     "sequences might be called recursively, because they are also functions,"
                     "so it's vital that we also push scopes when evaluating them"
                   push-stack
                   push-name Head
                   push-name <
:fv1-seq1-seq1-end ret-seq
                     "some clean-ups for anonymous functions"
                   push-name Filter Std
                   push-name Quicksort
                   make-list
                   push-name Head
                   push-list 1
                   push-name Tail
                   push-seq :fv1-seq1-seq2
:fv1-seq1-seq2-st  push-scope
                   push-stack
                   push-name Head
                   push-name >=
:fv1-seq1-seq2-end ret-seq
                   push-name Filter Std
                   push-name Quicksort
:fv1-seq1-end      ret-seq
                   mchend
:fv1-m1-p2! :fv1-m1-end :fv1-end ret
                     "ret is responsible for cleaning up, for example, copying TOS to"
                     "parent stack, popping scope, popping data stack, etc."
                   push-seq :seq1
:seq1-st           push-scope
                   push-stack
                   make-list
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
