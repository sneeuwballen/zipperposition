
INTERFACE_FILES = $(shell find src -name '*.mli')
IMPLEMENTATION_FILES = $(shell find src -name '*.ml')
TARGET = zipperposition.native
OPTIONS = -use-ocamlfind -use-jocaml

all:
	ocamlbuild $(OPTIONS) -tag debug src/$(TARGET)
prod:
	ocamlbuild $(OPTIONS) -tag noassert src/$(TARGET)

profile:ocamlbuild $(OPTIONS)
	ocamlbuild $(OPTIONS) -tags debug,profile src/$(TARGET)
byte:
	ocamlbuild $(OPTIONS) -tags debug src/zipperposition.byte

tests: all
	ocamlbuild $(OPTIONS) -tag debug -I src tests/tests.native

doc:
	ocamlbuild $(OPTIONS) src/zipperposition.docdir/index.html

clean:
	ocamlbuild -clean

tags:
	ctags $(IMPLEMENTATION_FILES) $(INTERFACE_FILES)

.PHONY: all profile clean tags doc tests
