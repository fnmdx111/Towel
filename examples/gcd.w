"bind Greatest-common-divisor fun X(Int) Y,"
"  (- if=0 X,"
"     if>0 (Y X - Y Greatest-common-divisor),"
"          (X X Y - Greatest-common-divisor))"
"then (42 24 Greatest-common-divisor)."


                            push-new-scope
                            bind Greatest-common-divisor
                            make-fun :fv1
                            fun-arg X
                            type Int
                            fun-arg Y
:fv1-st                     copy-from-parent-stack 2
                            push-new-scope
                            pushmake-seq :fv1-seq1
:fv1-seq1-st                push-name -
                            jez :fv1-seq1-j1
:fv1-seq1-j1                push-name X
:fv1-end1                   ret
:fv1-seq1-j1!               jgz :fv1-seq1-j2
:fv1-seq1-j2                pushmake-seq :fv1-seq1-seq1
:fv1-seq1-seq1-st           push-name Y
                            push-name X
                            push-name -
                            push-name Y
                            push-name Greatest-common-divisor
:fv1-end2                   ret
:fv1-seq1-seq1-end :fv1-seq1-j2! makepush-seq :fv1-seq1-seq2
:fv1-seq1-seq2-st           push-name X
                            push-name X
                            push-name Y
                            push-name -
                            push-name Greatest-common-divisor
:fv1-seq1-seq2-end :fv1-seq1-end :fv1-end3 ret
                            make-seq :seq1
:seq1                       push-lit-int 42
                            push-lit-int 24
                            push-name Greatest-common-divisor
:seq1-end                   terminate
