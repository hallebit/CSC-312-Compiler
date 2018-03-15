all: 
	ocamlc -o compiler ./lang.ml ./lexer.ml ./parser.ml ./compiler.ml

cleanup: 
	rm -f *.cmi *.cmo ../tests/*.result
clean: 
	rm -f *.cmi *.cmo ../tests/*.result compiler 

test:
	rm -f ../tests/*.result
	ocamlc -o compiler ./lang.ml ./lexer.ml ./parser.ml ./compiler.ml
	sh ../tests/test.sh