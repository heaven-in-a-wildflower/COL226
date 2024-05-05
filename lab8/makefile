make:
	ocamlc -c interpreter.ml
	ocamlyacc parser.mly
	ocamlc -c parser.mli
	ocamlc -c parser.ml
	ocamllex lexer.mll
	ocamlc -c lexer.ml
	ocamlc -c interface.ml
	ocamlfind ocamlc -linkpkg -package unix -o interface interpreter.cmo parser.cmo lexer.cmo interface.cmo

clean:
	rm -f lexer.ml parser.ml parser.mli
	rm -f *.c*
	rm -f interface
