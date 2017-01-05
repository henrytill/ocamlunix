OCB_FLAGS = -use-ocamlfind
OCB       = ocamlbuild $(OCB_FLAGS)

all: native byte

clean:
	$(OCB) -clean

native: sanity
	$(OCB) src/main.native

byte: sanity
	$(OCB) src/main.byte

sanity:
	ocamlfind query unix

.PHONY: all clean byte native sanity
