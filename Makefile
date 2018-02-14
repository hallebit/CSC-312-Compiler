all: 
	ocamlc read.ml -o read

cleanup: 
	rm -f *.cmi *.cmo tstOutput

clean: 
	rm -f *.cmi *.cmo tstOutput read

test:
	ocamlc read.ml -o read
	sh test.sh