
INTERFACE_FILES = $(shell find src -name '*.mli')
IMPLEMENTATION_FILES = $(shell find src -name '*.ml')
TARGET = zipperposition.native

all:
	ocamlbuild -use-ocamlfind -tag debug src/$(TARGET)
prod:
	ocamlbuild -use-ocamlfind -tag noassert src/$(TARGET)

profile:
	ocamlbuild -use-ocamlfind -tags debug,profile src/$(TARGET)
byte:
	ocamlbuild -use-ocamlfind -tags debug src/zipperposition.byte

tests: all
	ocamlbuild -use-ocamlfind -tag debug -I src tests/tests.native
profile_tests: all
	ocamlbuild -use-ocamlfind -tags debug,profile -I src tests/tests.native

doc:
	ocamlbuild -use-ocamlfind src/zipperposition.docdir/index.html

clean:
	ocamlbuild -clean

tags:
	ctags $(IMPLEMENTATION_FILES) $(INTERFACE_FILES)

.PHONY: all profile clean tags doc tests
