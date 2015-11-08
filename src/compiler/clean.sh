echo "Removing tuareg/merlin cache because stupid Waf won't build when
these caches exist."
rm -f *.cmo *.cmi
rm -f parser.ml parser.mli scanner.ml
