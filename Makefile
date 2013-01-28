
INTERFACE_FILES = $(shell find src -name '*.mli')
IMPLEMENTATION_FILES = $(shell find src -name '*.ml')
INSTALLDIR := /usr/bin/
TARGET = zipperposition.native
LIBS = unix,str
SUBMODULES = 
PWD = $(shell pwd)
OPTIONS = -libs $(LIBS)

# switch compilation module
MODE := prod

all: $(MODE) tests doc

debug: $(SUBMODULES)
	ocamlbuild $(OPTIONS) -tag debug src/$(TARGET)

prod: $(SUBMODULES) tests
	ocamlbuild $(OPTIONS) -tag noassert src/$(TARGET)

profile: $(SUBMODULES) tests
	ocamlbuild $(OPTIONS) -tags debug,profile src/$(TARGET)

byte: $(SUBMODULES) tests
	ocamlbuild $(OPTIONS) -tags debug src/zipperposition.byte

# build tests
tests: $(SUBMODULES)
	ocamlbuild $(OPTIONS) -tag debug -I src tests/tests.native

# build documentation
doc:
	ocamlbuild $(OPTIONS) -I src zipperposition.docdir/index.html

# cleanup build
clean:
	ocamlbuild -clean

# install the main binary
install: all
	cp $(TARGET) $(INSTALLDIR)/zipperposition

tags:
	ctags $(IMPLEMENTATION_FILES) $(INTERFACE_FILES)

.PHONY: all debug prod profile clean tags doc tests install

