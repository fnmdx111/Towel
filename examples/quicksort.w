"bind Quicksort fun L,"
"  match [], [];"
"        Head Tail ::,"
"          (Tail (Head <) Filter\Std Quicksort"
"           [Head]"
"           Tail (Head >=) Filter\Std Quicksort)"
"then ([5 4 3 2 1] Quicksort Print-list)."

push-stack
push-scope
make-fun :2418f516d-0-st
bind 0
jump :2418f516d-0-real-end
:2418f516d-0-st push-stack
push-scope
fun-arg 1
patpush-list
end-list
match :2418f516d-1-p0!
push-list
end-list
jump :2418f516d-1-end
:2418f516d-1-p0! patpush-name 2L
patpush-name 3L
patpush-name 4L
match :2418f516d-1-p1!
push-name 3L
push-fun :2418f516d-2-st
jump :2418f516d-2-real-end
:2418f516d-2-st push-stack
push-scope
push-name 2L
push-name -1L
:2418f516d-2-end pop-scope
ret
:2418f516d-2-real-end
push-name -1L
push-name 0L
push-list
push-name 2L
end-list
push-name 3L
push-fun :2418f516d-3-st
jump :2418f516d-3-real-end
:2418f516d-3-st push-stack
push-scope
push-name 2L
push-name -1L
:2418f516d-3-end pop-scope
ret
:2418f516d-3-real-end
push-name -1L
push-name 0L
push-name -1L
push-name -1L
jump :2418f516d-1-end
:2418f516d-1-p1!
:2418f516d-1-end :2418f516d-0-end ret
:2418f516d-0-real-end
push-list
push-fint 5
push-fint 4
push-fint 3
push-fint 2
push-fint 1
end-list
push-name 0L
terminate
