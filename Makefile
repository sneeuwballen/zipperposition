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
SUBMODULES = containers datalog
PACKAGES = 
CAML_OPTS = 
CAML_LIBS = str nums unix

# build targets
TARGETS_LIB = src/lib.cmxa src/lib.cma src/lib.cmi
TARGETS_BIN = src/zipperposition.native
TARGETS_TEST = tests/tests.native
TARGET_PLUGINS = $(PLUGIN_FILES:%.ml=%.cmxs)

# output, ready to install
BINARY = zipperposition.native
LIBS = $(addprefix _build/,$(TARGETS_LIB))
PLUGINS = $(addprefix _build/,$(TARGET_PLUGINS))
INSTALLDIR=/usr/bin/

PWD = $(shell pwd)

#INCLUDES = -I,$(PWD)/datalog/_build,-I,$(PWD)/sequence/_build/
INCLUDES = -I,src,-I,datalog,-I,containers
#OPTIONS = -cflags $(INCLUDES) -lflags $(INCLUDES) -libs $(LIBS) -I src
#OPTIONS = $(WITH_PACKAGES) $(WITH_LIBS) -I src -cflags $(INCLUDES) -lflags $(INCLUDES)
#OPTIONS = -cflags $(INCLUDES) -lflags $(INCLUDES) -libs $(LIBS) -I src
#OPTIONS = -use-ocamlfind -I src $(PP) -classic-display
OPTIONS = -use-ocamlfind -I src $(PP) -X plugins -yaccflag -v
#OPTIONS_LIB = -I src -cflags $(INCLUDES) -lflags $(INCLUDES)
OPTIONS_LIB = -I src -cflags $(INCLUDES) 

# switch compilation module
MODE ?= debug
ifeq ($(MODE),debug)
	TAGS=-tag debug
	CAML_OPTS = -g
endif
ifeq ($(MODE),profile)
	TAGS=-tags debug,profile
	CAML_OPTS = -g -p
endif
ifeq ($(MODE),prod)
	TAGS=-tag noassert
	CAML_OPTS = -noassert
endif

# all: bin + tests + plugins

all: bin tests plugins

bin: src/const.ml
	ocamlbuild $(OPTIONS) $(TAGS) $(TARGETS_LIB) $(TARGETS_BIN)

tests: bin
	ocamlbuild $(OPTIONS) $(TAGS) $(TARGETS_TEST)

byte:
	ocamlbuild $(OPTIONS) -tags debug src/zipperposition.byte

BUILD_PATH = $(PWD)/_build/src/
PLUGIN_OPTIONS = -cflags -I,$(BUILD_PATH) -lib lib
plugins: bin
	mkdir -p plugins/std/
	for f in $(wildcard plugins/*.ml) ; do \
	    ocamlopt $(CAML_OPTS) -c -I _build/src $$f -o plugins/$$(basename $$f .ml).cmx; \
	    ocamlopt $(CAML_OPTS) -shared plugins/$$(basename $$f .ml).cmx -o plugins/std/$$(basename $$f .ml).cmxs; \
	done
	@echo plugins compiled.

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
	rm plugins/*.cm* plugins/std/*.cmxs || true

# install the main binary
install: bin plugins
	mkdir -p $(ZIPPERPOSITION_HOME)
	mkdir -p $(ZIPPERPOSITION_HOME)/plugins/
	cp builtin.theory $(ZIPPERPOSITION_HOME)/
	cp $(BINARY) $(INSTALLDIR)/zipperposition
	cp $(LIBS) $(ZIPPERPOSITION_HOME)/
	cp $(PLUGINS) $(ZIPPERPOSITION_HOME)/plugins/
	./$(BINARY) -kb $(ZIPPERPOSITION_HOME)/kb -kb-load builtin.theory /dev/null || true
	@echo done.

tags:
	otags $(IMPLEMENTATION_FILES) $(INTERFACE_FILES)

dot:
	for i in *.dot; do dot -Tsvg "$$i" > "$$( basename $$i .dot )".svg; done

.PHONY: all debug prod profile clean tags doc tests install plugins dot

# libraries

containers:
	make -C containers

datalog:
	make -C datalog

.PHONY: containers datalog
