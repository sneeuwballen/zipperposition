
VERSION=0.1.1
PP = 'sed -r s/ZIPPERPOSITION_VERSION/$(VERSION)/g'

INTERFACE_FILES = $(shell find src -name '*.mli')
IMPLEMENTATION_FILES = $(shell find src -name '*.ml')
INSTALLDIR ?= /usr/bin/
BINARY = zipperposition.native
TARGETS = src/zipperposition.native tests/tests.native zipperposition.docdir/index.html
LIBS = unix,str
SUBMODULES = 
PWD = $(shell pwd)
OPTIONS = -libs $(LIBS) -I src -pp $(PP)

# switch compilation module
MODE := prod

all: $(MODE)

debug: $(SUBMODULES)
	ocamlbuild $(OPTIONS) -tag debug $(TARGETS)

prod: $(SUBMODULES) tests
	ocamlbuild $(OPTIONS) -tag noassert $(TARGETS)

profile: $(SUBMODULES) tests
	ocamlbuild $(OPTIONS) -tags debug,profile $(TARGETS)

# just build bytecode
byte: $(SUBMODULES) tests
	ocamlbuild $(OPTIONS) -tags debug src/zipperposition.byte

# cleanup build
clean:
	ocamlbuild -clean

# install the main binary
install: all
	cp $(BINARY) $(INSTALLDIR)/zipperposition

tags:
	ctags $(IMPLEMENTATION_FILES) $(INTERFACE_FILES)

.PHONY: all debug prod profile clean tags doc tests install

