OCB_FLAGS = -use-ocamlfind -I src
OCB       = ocamlbuild $(OCB_FLAGS)

all: native byte

clean:
	$(OCB) -clean

native: sanity
	$(OCB) file_copy.native

byte: sanity
	$(OCB) file_copy.byte

profile: sanity
	$(OCB) -tag profile file_copy.native

debug: sanity
	$(OCB) -tag debug file_copy.byte

sanity:
	ocamlfind query unix

test: native
	./file_copy.native

.PHONY: all clean byte native profile debug sanity test
