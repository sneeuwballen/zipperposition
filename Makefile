# Main makefile for zipperposition

INTERFACE_FILES = $(shell find src -name '*.mli')
IMPLEMENTATION_FILES = $(shell find src -name '*.ml')
PLUGIN_FILES = $(shell find plugins/ -name 'ext_*.ml')
INSTALLDIR ?= /usr/bin/
SUBMODULES = containers

TARGET_PLUGINS = $(PLUGIN_FILES:%.ml=%.cmxs)

# output, ready to install
BINARY = zipperposition.native
LIBS = $(addprefix _build/,$(TARGETS_LIB))
PLUGINS = $(addprefix _build/,$(TARGET_PLUGINS))
INSTALLDIR=/usr/bin/

PWD = $(shell pwd)

# all: bin + tests + plugins

all: bin plugins

bin: 
	ocaml setup.ml -all

tests: bin
	ocaml setup.ml -test

doc:
	ocaml setup.ml -doc

BUILD_PATH = $(PWD)/_build/src/
PLUGIN_OPTIONS = -cflags -I,$(BUILD_PATH) -lib lib
plugins: bin
	mkdir -p plugins/std/
	for f in $(wildcard plugins/*.ml) ; do \
	    ocamlfind ocamlopt $(CAML_OPTS) -c -I _build/src -package logtk $$f \
	    	-o plugins/$$(basename $$f .ml).cmx; \
	    ocamlfind ocamlopt $(CAML_OPTS) -package logtk \
	    	-shared plugins/$$(basename $$f .ml).cmx \
	    	-o plugins/std/$$(basename $$f .ml).cmxs; \
	done
	@echo plugins compiled.

clean:
	rm -f src/const.ml || true
	rm plugins/*.cm* plugins/std/*.cmxs || true
	ocaml setup.ml -clean

# install the main binary
install:
	ocaml setup.ml -install

reinstall:
	ocaml setup.ml -reinstall

uninstall:
	ocaml setup.ml -uninstall

tags:
	otags $(IMPLEMENTATION_FILES) $(INTERFACE_FILES)

dot:
	for i in *.dot; do dot -Tsvg "$$i" > "$$( basename $$i .dot )".svg; done

.PHONY: all debug prod profile clean tags doc tests install plugins dot

