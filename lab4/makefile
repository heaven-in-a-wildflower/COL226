.PHONY: run

run:
	@ocamllex lexer.mll
	@ocamlyacc parser.mly
	@ocamlc -c structure.ml
	@ocamlc -c parser.mli
	@ocamlc -c lexer.ml
	@ocamlc -c parser.ml
	@ocamlc -c main.ml
	@ocamlc -o main structure.cmo parser.cmo lexer.cmo main.cmo
	@./main



