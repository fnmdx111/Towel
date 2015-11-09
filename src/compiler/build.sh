echo "Building cache for tuareg or merlin..."
python3 ../tasm/ccg.py . ../tasm/scanner.mll.p
ocamlfind ocamlc -package Batteries,Stdint,Extlib,Sha -linkpkg -c ast.mli
ocamllex scanner.mll
menhir parser.mly
ocamlfind ocamlc -package Batteries,Stdint,Extlib,Sha -linkpkg -c tasm_ast.mli
ocamllex tasm_scanner.mll
menhir tasm_parser.mly
ocamlfind ocamlc -package Batteries,Stdint,Extlib,Sha -linkpkg -c cseg.ml
ocamlfind ocamlc -package Batteries,Stdint,Extlib,Sha -linkpkg -c common.ml
ocamlfind ocamlc -package Batteries,Stdint,Extlib,Sha -linkpkg -c exc.ml
ocamlfind ocamlc -package Batteries,Stdint,Extlib,Sha -linkpkg -c parser.mli
ocamlfind ocamlc -package Batteries,Stdint,Extlib,Sha -linkpkg -c scanner.ml
ocamlfind ocamlc -package Batteries,Stdint,Extlib,Sha -linkpkg -c tasm_parser.mli
ocamlfind ocamlc -package Batteries,Stdint,Extlib,Sha -linkpkg -c tasm_scanner.ml
ocamlfind ocamlc -package Batteries,Stdint,Extlib,Sha -linkpkg -c switches.ml
ocamlfind ocamlc -package Batteries,Stdint,Extlib,Sha -linkpkg -c scoping.ml
ocamlfind ocamlc -package Batteries,Stdint,Extlib,Sha -linkpkg -c parser.ml
ocamlfind ocamlc -package Batteries,Stdint,Extlib,Sha -linkpkg -c tasm_parser.ml
ocamlfind ocamlc -package Batteries,Stdint,Extlib,Sha -linkpkg -c tasm_stringify.ml
ocamlfind ocamlc -package Batteries,Stdint,Extlib,Sha -linkpkg -c unlabel.ml
ocamlfind ocamlc -package Batteries,Stdint,Extlib,Sha -linkpkg -c asm.ml
ocamlfind ocamlc -package Batteries,Stdint,Extlib,Sha -linkpkg -c towel.ml
