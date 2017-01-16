OCB_FLAGS = -use-ocamlfind
OCB       = ocamlbuild $(OCB_FLAGS)

all: native byte

modules:
	$(OCB) src/circular_buffer.cmo

clean:
	$(OCB) -clean

native: sanity modules
	$(OCB) src/main.native

byte: sanity modules
	$(OCB) src/main.byte

test: all
	$(OCB) test/all_tests.byte
	ocamlrun all_tests.byte

sanity:
	ocamlfind query unix alcotest

.PHONY: all lib clean byte native sanity test
