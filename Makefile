# Main makefile for zipperposition

VERSION=0.2
ZIPPERPOSITION_HOME ?= $(HOME)/.zipperposition/

PP = 'sed -e "s/ZIPPERPOSITION_VERSION/$(VERSION)/g" -e "s+ZIPPERPOSITION_HOME+$(ZIPPERPOSITION_HOME)+g"'


INTERFACE_FILES = $(shell find src -name '*.mli')
IMPLEMENTATION_FILES = $(shell find src -name '*.ml')
INSTALLDIR ?= /usr/bin/
BINARY = zipperposition.native
TARGETS_LIB = src/lib.cmxa src/lib.cma 
TARGETS_BIN = src/zipperposition.native
TARGETS_TEST = tests/tests.native
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
ifeq ($(MODE),debug)
	TAGS=-tag debug
endif
ifeq ($(MODE),profile)
	TAGS=-tags debug,profile
endif
ifeq ($(MODE),prod)
	TAGS=-tag noassert
endif

all: native lib tests

lib:
	ocamlbuild $(OPTIONS_LIB) $(TAGS) $(TARGETS_LIB)

native:
	ocamlbuild $(OPTIONS) $(TAGS) $(TARGETS_BIN)

tests: lib
	ocamlbuild $(OPTIONS) $(TAGS) $(TARGETS_TEST)

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
install: native
	mkdir -p $(ZIPPERPOSITION_HOME)
	cp builtin.theory $(ZIPPERPOSITION_HOME)/
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
