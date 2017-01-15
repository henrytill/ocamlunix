OCB_FLAGS = -use-ocamlfind
OCB       = ocamlbuild $(OCB_FLAGS)

all: native byte

lib:
	$(OCB) src/circular_buffer.cma

clean:
	$(OCB) -clean

native: sanity
	$(OCB) src/tail.native

byte: sanity
	$(OCB) src/tail.byte

test:
	$(OCB) test/test.byte
	ocamlrun test.byte

sanity:
	ocamlfind query unix alcotest

.PHONY: all lib clean byte native sanity test
