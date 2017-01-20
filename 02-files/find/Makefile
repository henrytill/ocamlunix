OCB_FLAGS = -use-ocamlfind
OCB       = ocamlbuild $(OCB_FLAGS)

FILES   = find exercise1 getcwd
NATIVES = $(addsuffix .native, $(FILES))
BYTES   = $(addsuffix .byte,   $(FILES))

all: $(NATIVES) $(BYTES)

clean:
	$(OCB) -clean

%.native: sanity
	$(OCB) $@

%.byte: sanity
	$(OCB) $@

profile: sanity
	$(OCB) -tag profile $(NATIVES)

debug: sanity
	$(OCB) -tag debug $(BYTES)

sanity:
	ocamlfind query unix

test: $(NATIVES)
	./getcwd.native

.PHONY: all clean profile debug sanity test
