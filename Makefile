# Main makefile for zipperposition

VERSION=0.2
ZIPPERPOSITION_HOME ?= $(HOME)/.zipperposition/

%.ml: %.mlp
	sed -e "s/ZIPPERPOSITION_VERSION/$(VERSION)/g" \
		-e "s+ZIPPERPOSITION_HOME+$(ZIPPERPOSITION_HOME)+g" $< > $@

INTERFACE_FILES = $(shell find src -name '*.mli')
IMPLEMENTATION_FILES = $(shell find src -name '*.ml')
PLUGIN_FILES = $(shell find plugins -name '*.ml')
TARGETS_LIB = src/lib.cmxa src/lib.cma src/lib.cmi
TARGETS_BIN = src/zipperposition.native 
TARGETS_TEST = tests/tests.native
TARGET_PLUGINS = $(PLUGIN_FILES:%.ml=%.cmxs)

BINARY = zipperposition.native
LIBS = $(addprefix _build/,$(TARGETS_LIB))
PLUGINS = $(addprefix _build/,$(TARGET_PLUGINS))
INSTALLDIR=/usr/bin/

PWD = $(shell pwd)
#OPTIONS = -cflags $(INCLUDES) -lflags $(INCLUDES) -libs $(LIBS) -I src
#OPTIONS = -use-ocamlfind -I src $(PP) -classic-display
OPTIONS = -use-ocamlfind -I src $(PP) -X plugins -yaccflag -v

# switch compilation module
MODE ?= debug
ifeq ($(MODE),debug)
	TAGS=-tag debug
	CAMLOPTS = -g
endif
ifeq ($(MODE),profile)
	TAGS=-tags debug,profile
	CAMLOPTS = -g -p
endif
ifeq ($(MODE),prod)
	TAGS=-tag noassert
	CAMLOPTS = -noassert
endif

# all: bin + tests + plugins

all: bin tests plugins

bin: src/const.ml
	ocamlbuild $(OPTIONS) $(TAGS) $(TARGETS_LIB) $(TARGETS_BIN)

tests:
	ocamlbuild $(OPTIONS) $(TAGS) $(TARGETS_TEST)

BUILD_PATH = $(PWD)/_build/src/
PLUGIN_OPTIONS = -cflags -I,$(BUILD_PATH) -lib lib
plugins: bin
	mkdir -p plugins/std/
	for f in $(wildcard plugins/*.ml) ; do \
	    ocamlopt $(CAMLOPTS) -c -I _build/src $$f -o plugins/$$(basename $$f .ml).cmx; \
	    ocamlopt $(CAMLOPTS) -shared plugins/$$(basename $$f .ml).cmx -o plugins/std/$$(basename $$f .ml).cmxs; \
	done
	@echo plugins compiled.

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

.PHONY: all bin clean tags doc dot tests plugins

.PHONY: containers
