all: 
	ocamlc -o compiler ./lang.ml ./lexer.ml ./parser.ml ./compiler.ml

cleanup: 
	rm -f *.cmi *.cmo ../tests/OutputResult

clean: 
	rm -f *.cmi *.cmo ../tests/OutputResult compiler 

test:
	ocamlc -o compiler lang.ml lexer.ml parser.ml compiler.ml
	sh ../tests/test.sh