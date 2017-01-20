OCB_FLAGS = -use-ocamlfind
OCB       = ocamlbuild $(OCB_FLAGS)

all: native

clean:
	$(OCB) -clean

native: sanity
	$(OCB) src/read_passwd.native

byte: sanity
	$(OCB) src/read_passwd.byte

sanity:
	ocamlfind query unix

.PHONY: all clean byte native sanity
