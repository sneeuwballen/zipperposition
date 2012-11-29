
INTERFACE_FILES = $(shell find src -name '*.mli')
IMPLEMENTATION_FILES = $(shell find src -name '*.ml')
TARGET = zipperposition.native

all:
	ocamlbuild -libs str,unix -tag debug src/$(TARGET)
prod:
	ocamlbuild -libs str,unix -tag noassert src/$(TARGET)

profile:
	ocamlbuild -libs str,unix -tags debug,profile src/$(TARGET)
byte:
	ocamlbuild -libs str,unix -tags debug src/zipperposition.byte

tests: all
	ocamlbuild -libs str,unix -tag debug -I src tests/tests.native
profile_tests: all
	ocamlbuild -libs str,unix -tags debug,profile -I src tests/tests.native

doc:
	ocamlbuild src/zipperposition.docdir/index.html

clean:
	ocamlbuild -clean

tags:
	ctags $(IMPLEMENTATION_FILES) $(INTERFACE_FILES)

.PHONY: all profile clean tags doc tests
