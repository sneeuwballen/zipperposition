# Main makefile for zipperposition

VERSION=0.2

PP = 'sed s/ZIPPERPOSITION_VERSION/$(VERSION)/g'


INTERFACE_FILES = $(shell find src -name '*.mli')
IMPLEMENTATION_FILES = $(shell find src -name '*.ml')
INSTALLDIR ?= /usr/bin/
BINARY = zipperposition.native
TARGETS_LIB = src/lib.cmxa src/lib.cma 
TARGETS = src/zipperposition.native tests/tests.native
LIBS = datalog
#SUBMODULES = datalog sequence
SUBMODULES = containers
PACKAGES = yojson zip str num

WITH_LIBS = $(addprefix -lib ,$(LIBS))
WITH_PACKAGES = $(addprefix -package ,$(PACKAGES))

PWD = $(shell pwd)
#INCLUDES = -I,$(PWD)/datalog/_build,-I,$(PWD)/sequence/_build/
INCLUDES = -I,src
#OPTIONS = -cflags $(INCLUDES) -lflags $(INCLUDES) -libs $(LIBS) -I src
OPTIONS = -use-ocamlfind $(WITH_PACKAGES) $(WITH_LIBS) -I src -cflags $(INCLUDES) -lflags $(INCLUDES) -pp $(PP)
#OPTIONS_LIB = -I src -cflags $(INCLUDES) -lflags $(INCLUDES)
OPTIONS_LIB = -use-ocamlfind -I src -cflags $(INCLUDES) 

# switch compilation module
MODE ?= debug

all: $(MODE)

debug:
	ocamlbuild $(OPTIONS_LIB) -tag debug $(TARGETS_LIB)
	ocamlbuild $(OPTIONS) -tag debug $(TARGETS)

prod:
	ocamlbuild $(OPTIONS_LIB) -tag noassert $(TARGETS_LIB)
	ocamlbuild $(OPTIONS) -tag noassert $(TARGETS)

profile:
	ocamlbuild $(OPTIONS_LIB) -tag debug,profile $(TARGETS_LIB)
	ocamlbuild $(OPTIONS) -tag debug,profile $(TARGETS)

byte:
	ocamlbuild $(OPTIONS) -tags debug src/zipperposition.byte

doc:
	ocamlbuild $(OPTIONS) src/zipperposition.docdir/index.html
	cd src; find . -iname '*.ml{,i}' | xargs ocamlfind ocamldoc \
		-I ../_build/src -I ../_build/containers -I ../_build/meta \
		-package yojson -package datalog -dot -o modules.dot
	cd src; find . -iname '*.ml{,i}' | xargs ocamlfind ocamldoc \
		-I ../_build/src -I ../_build/containers -I ../_build/meta \
		-package yojson -package datalog -man -d man/

clean:
	ocamlbuild -clean

# install the main binary
install: all
	cp $(BINARY) $(INSTALLDIR)/zipperposition

tags:
	otags $(IMPLEMENTATION_FILES) $(INTERFACE_FILES)

dot:
	for i in *.dot; do dot -Tsvg "$$i" > "$$( basename $$i .dot )".svg; done

.PHONY: all debug prod profile clean tags doc tests install dot

# libraries

containers:
	make -C containers

.PHONY: containers
