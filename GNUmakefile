DUNE = dune

.PHONY: all
all:
	@$(DUNE) build

.PHONY: test check
test check:
	@$(DUNE) test -f

.PHONY: fmt
fmt:
	@$(DUNE) fmt

.PHONY: clean
clean:
	@$(DUNE) clean
