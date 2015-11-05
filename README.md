The Towel Programming Language
====

Towel is a typed stack-based postfix-syntaxed general-purposed functional
language. A Towel compiler that compiles source code in Towel to Towel assembly
language (TAL) which runs on Towel virtual machine written in OCaml. More
robust design and implementation will be scheduled. A TAL to C compiler is also
scheduled.


Build
----

We threw away the old Make building system and changed to Waf. Run

    waf configure build

to build for Towel compiler executable `towel` for now.

If you want some documentations, e.g. manuals, run

    waf configure build --docs

If you want to build native executable instead of the one of OCaml bytecodes,
run

    waf configure build --native


License
----

This project is licensed under GPLv3.
