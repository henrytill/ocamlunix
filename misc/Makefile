OCB_FLAGS = -use-ocamlfind
OCB       = ocamlbuild $(OCB_FLAGS)

all: lib

clean:
	$(OCB) -clean

lib:
	$(OCB) lib/misc.cma
	$(OCB) lib/misc.cmxa
	$(OCB) lib/misc.cmxs

sanity:
	ocamlfind query unix

.PHONY: all clean lib sanity
