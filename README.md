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

To build Towel virtual machine, Run

    wal configure build --tvm


Don't panic!
----

See `towel -h`.

Roadmap
----

* Implement some key standard libraries. Priority 5.

* Implement type checking algorithm! Priority 4.

* The module system needs to be detailed designed. Priority 3.5.
This is done by now. Parts missing: libpath mechanism, and maybe something else.

* A script that generates std.w and std.e. Module `Std` generally covers all the
assembly instructions that aren't covered in `asm`. Priority 4.

The Future
----

Of course the TOWEL-C COMPILER like the cool guys!!! (And of course not javascript, it just sucks.)
Maybe a bytecode compiler for a VM in C.

Oh, and algebraic data type. I'll have to do a lot of thinking.

License
----

This project is licensed under GPLv3.
