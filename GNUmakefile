.SUFFIXES:

DUNE = dune

.PHONY: all
all: build

.PHONY: build
build:
	$(DUNE) $@ @all

.PHONY: test check
test check:
	$(DUNE) test -f

.PHONY: fmt clean
fmt clean:
	$(DUNE) $@
