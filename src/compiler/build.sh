echo "Building cache for tuareg or merlin..."
ocamlfind ocamlc -package Batteries,Stdint,Extlib,Sha -linkpkg -c ast.mli
ocamllex scanner.mll
menhir parser.mly
ocamlfind ocamlc -package Batteries,Stdint,Extlib,Sha -linkpkg -c cseg.ml
ocamlfind ocamlc -package Batteries,Stdint,Extlib,Sha -linkpkg -c common.ml
ocamlfind ocamlc -package Batteries,Stdint,Extlib,Sha -linkpkg -c exc.ml
ocamlfind ocamlc -package Batteries,Stdint,Extlib,Sha -linkpkg -c parser.mli
ocamlfind ocamlc -package Batteries,Stdint,Extlib,Sha -linkpkg -c scanner.ml
ocamlfind ocamlc -package Batteries,Stdint,Extlib,Sha -linkpkg -c switches.ml
ocamlfind ocamlc -package Batteries,Stdint,Extlib,Sha -linkpkg -c parser.ml
ocamlfind ocamlc -package Batteries,Stdint,Extlib,Sha -linkpkg -c asm.ml
ocamlfind ocamlc -package Batteries,Stdint,Extlib,Sha -linkpkg -c towel.ml
