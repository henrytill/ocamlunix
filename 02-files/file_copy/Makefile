OCB_FLAGS = -use-ocamlfind -I src
OCB       = ocamlbuild $(OCB_FLAGS)

TESTDIR := $(shell mktemp -d /tmp/file_copy.XXXXXXXXXX)

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
	@echo
	@echo "========================================="
	@echo "Test Directory: $(TESTDIR)"
	@echo "========================================="
	mkdir -p $(TESTDIR)
	dd if=/dev/urandom of=$(TESTDIR)/source bs=1M count=1
	@echo
	@echo "======="
	@echo "Test 1:"
	@echo "======="
	./file_copy.native $(TESTDIR)/source $(TESTDIR)/copy
	cmp -s $(TESTDIR)/source $(TESTDIR)/copy
	@echo "PASSED!"
	@echo
	@echo "======="
	@echo "Test 2:"
	@echo "======="
	./file_copy.native -a $(TESTDIR)/source $(TESTDIR)/copy
	cat $(TESTDIR)/source $(TESTDIR)/source | cmp -s $(TESTDIR)/copy
	@echo "PASSED!"
	@echo
	@echo "All tests passed!"
	@echo
	@echo "Cleaning up..."
	@rm -r $(TESTDIR)

.PHONY: all clean byte native profile debug sanity test
