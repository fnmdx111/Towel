The Towel Programming Language
====

Towel is a typed stack-based postfix-syntaxed general-purposed functional
language. A Towel compiler that compiles source code in Towel to Towel assembly
language (TAL) which runs on Towel virtual machine written in OCaml. More
robust design and implementation will be scheduled. A TAL to C compiler is also
scheduled.


Build
----

We threw away the make building system and changed to Waf. Run

    waf configure --docs build

or if you don't want the documentations,

    waf configure build


License
----

This project is licensed under GPLv3.
