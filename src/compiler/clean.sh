echo "Removing tuareg/merlin cache because stupid Waf won't build when
these caches exist."
rm -f *.cmo *.cmi
rm -f parser.ml parser.mli scanner.ml
rm -f parser.conflicts parser.automaton
rm -f tasm*
rm -f src/compiler/*.cmo src/compiler/*.cmi
rm -f src/compiler/parser.ml src/compiler/parser.mli src/compiler/scanner.ml
rm -f src/compiler/parser.conflicts src/compiler/parser.automaton
rm -f src/compiler/tasm
