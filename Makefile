
INTERFACE_FILES = $(shell find src -name '*.mli')

IMPLEMENTATION_FILES = $(shell find src -name '*.ml')

all:
	ocamlbuild -libs str,unix -tag debug src/main.native
prod:
	ocamlbuild -libs str,unix -tag noassert src/main.native

profile:
	ocamlbuild -libs str,unix -tags debug,profile src/main.native
byte:
	ocamlbuild -libs str,unix -tags debug src/main.byte

tests: all
	ocamlbuild -libs str,unix -tag debug -I src tests/tests.native
profile_tests: all
	ocamlbuild -libs str,unix -tags debug,profile -I src tests/tests.native

doc:
	ocamlbuild src/main.docdir/index.html

clean:
	ocamlbuild -clean

tags:
	ctags $(IMPLEMENTATION_FILES) $(INTERFACE_FILES)

.PHONY: all profile clean tags doc tests
