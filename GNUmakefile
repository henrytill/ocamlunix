.SUFFIXES:

DUNE = dune

.PHONY: all
all: build

.PHONY: test check
test check:
	$(DUNE) test -f

.PHONY: build fmt clean
build fmt clean:
	$(DUNE) $@
