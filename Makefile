# Main makefile for zipperposition

VERSION=0.3
ZIPPERPOSITION_HOME ?= $(HOME)/.zipperposition/

%.ml: %.mlp
	sed -e "s/ZIPPERPOSITION_VERSION/$(VERSION)/g" \
		-e "s+ZIPPERPOSITION_HOME+$(ZIPPERPOSITION_HOME)+g" $< > $@

INTERFACE_FILES = $(shell find src -name '*.mli')
IMPLEMENTATION_FILES = $(shell find src -name '*.ml')
INSTALLDIR ?= /usr/bin/
BINARY = zipperposition.native
TARGETS_LIB = src/lib.cmxa src/lib.cma 
TARGETS_BIN = src/zipperposition.native
TARGETS_TEST = tests/tests.native
SUBMODULES = containers datalog
PACKAGES = 
LIBS = str nums unix

WITH_LIBS = $(addprefix -lib ,$(LIBS))
WITH_PACKAGES = $(addprefix -package ,$(PACKAGES))

PWD = $(shell pwd)
#INCLUDES = -I,$(PWD)/datalog/_build,-I,$(PWD)/sequence/_build/
INCLUDES = -I,src,-I,datalog
#OPTIONS = -cflags $(INCLUDES) -lflags $(INCLUDES) -libs $(LIBS) -I src
OPTIONS = $(WITH_PACKAGES) $(WITH_LIBS) -I src -cflags $(INCLUDES) -lflags $(INCLUDES)
#OPTIONS_LIB = -I src -cflags $(INCLUDES) -lflags $(INCLUDES)
OPTIONS_LIB = -I src -cflags $(INCLUDES) 

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

lib: src/const.ml
	ocamlbuild $(OPTIONS_LIB) $(TAGS) $(TARGETS_LIB)

native: src/const.ml
	ocamlbuild $(OPTIONS) $(TAGS) $(TARGETS_BIN)

tests: lib
	ocamlbuild $(OPTIONS) $(TAGS) $(TARGETS_TEST)

byte:
	ocamlbuild $(OPTIONS) -tags debug src/zipperposition.byte

doc:
	ocamlbuild $(OPTIONS) src/zipperposition.docdir/index.html
	cd src; find . -iname '*.ml{,i}' | xargs ocamlfind ocamldoc \
		-I ../_build/src -I ../_build/containers -I ../_build/meta \
		-dot -o modules.dot
	cd src; find . -iname '*.ml{,i}' | xargs ocamlfind ocamldoc \
		-I ../_build/src -I ../_build/containers -I ../_build/meta \
		-man -d man/

clean:
	rm -f src/const.ml || true
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

datalog:
	make -C datalog

.PHONY: containers datalog
