"bind Greatest-common-divisor fun X(Int) Y,"
"  (@ - if=0 X,"
"       if>0 (Y X - Y Greatest-common-divisor),"
"            (X X Y - Greatest-common-divisor))"
"then (42 24 Greatest-common-divisor)."


                            push-scope
			    push-stack
                            bind Greatest-common-divisor
                            make-fun :fv1
:fv1-st                     push-scope
                            push-stack
                            fun-arg Y
                              "note the order is different here"
                            fun-arg X
                            push-name X
                            push-name Y
                            push-shared-seq :fv1-seq1
                              "remember that this seq shares the same stack"
                              "with its caller"
:fv1-seq1-st                push-scope
                            push-stack
                            push-name -
                            jez :fv1-seq1-j1
                              "if TOS is equal to zero, jump to :fv1-seq1-j1,"
                              "otherwise :fv1-seq1-j1!"
:fv1-seq1-j1                push-name X
                            jend
                              "something akin to ret or ret-seq, directly jump"
                              "to the end of this if form, i.e. :fv1-seq1-j1-end"
:fv1-seq1-j1!               jgz :fv1-seq1-j2
:fv1-seq1-j2                push-seq :fv1-seq1-seq1
:fv1-seq1-seq1-st           push-scope
                            push-stack
                            push-name Y
                            push-name X
                            push-name -
                            push-name Y
                            push-name Greatest-common-divisor
:fv1-seq1-seq1-end          ret-seq
                            jend
:fv1-seq1-j2!               push-seq :fv1-seq1-seq2
:fv1-seq1-seq2-st           push-scope
                            push-stack
                            push-name X
                            push-name X
                            push-name Y
                            push-name -
                            push-name Greatest-common-divisor
:fv1-seq1-seq2-end          ret-seq
                            jend
:fv1-seq1-j2-end :fv1-seq1-j1-end :fv1-seq1-end ret-seq
:fv1-end3                   ret
                            push-seq :seq1
:seq1-st                    push-scope
                            push-stack
                            push-int 42
                            push-int 24
                            push-name Greatest-common-divisor
:seq1-end                   ret-seq
                            terminate
