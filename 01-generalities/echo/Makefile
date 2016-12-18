OCB_FLAGS = -use-ocamlfind -I src
OCB       = ocamlbuild $(OCB_FLAGS)

all: native byte

clean:
	$(OCB) -clean

native: sanity
	$(OCB) echo.native

byte: sanity
	$(OCB) echo.byte

profile: sanity
	$(OCB) -tag profile echo.native

debug: sanity
	$(OCB) -tag debug echo.byte

sanity:
	ocamlfind query unix

test: native
	./echo.native '[1, 2, "three", {"four": 4}]'

.PHONY: all clean byte native profile debug sanity test
