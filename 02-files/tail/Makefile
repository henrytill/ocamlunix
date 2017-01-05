OCB_FLAGS = -use-ocamlfind
OCB       = ocamlbuild $(OCB_FLAGS)

all: native byte

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

.PHONY: all clean byte native sanity test
