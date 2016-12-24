OCB_FLAGS = -use-ocamlfind -I src
OCB       = ocamlbuild $(OCB_FLAGS)

all: native byte

clean:
	$(OCB) -clean

native: sanity
	$(OCB) find.native

byte: sanity
	$(OCB) find.byte

profile: sanity
	$(OCB) -tag profile find.native

debug: sanity
	$(OCB) -tag debug find.byte

sanity:
	ocamlfind query unix

test: native
	./find.native

.PHONY: all clean byte native profile debug sanity test
