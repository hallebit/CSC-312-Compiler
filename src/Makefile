OCBFLAGS :=
OCB := ocamlbuild $(OCBFLAGS)

.PHONY: all debug clean top

all: ./compiler.byte
debug: all compiler.cma

%.cma: .FORCE
	$(OCB) $@

%.cmxa: .FORCE
	$(OCB) $@

%.native: .FORCE
	$(OCB) $@

%.p.native: .FORCE
	$(OCB) $@

%.byte: .FORCE
	$(OCB) $@

.FORCE:

cleanup: 
	rm -f *.cmi *.cmo ../tests/*.result ./compiler.byte

clean: 
	$(OCB) -clean

top: compiler.cma
	utop

test: 
	sh ../tests/test.sh
	rm -f ../tests/*.result