
INTERFACE_FILES = $(shell find src -name '*.mli')
IMPLEMENTATION_FILES = $(shell find src -name '*.ml')
PLUGIN_FILES = $(shell find plugins -name '*.ml')
TARGETS_LIB = src/lib.cmxa src/lib.cma src/lib.cmi
TARGETS_BIN = src/zipperposition.native 
TARGETS_TEST = tests/tests.native
PLUGINS = $(PLUGIN_FILES:%.ml=%.cmxs)

PWD = $(shell pwd)
#OPTIONS = -cflags $(INCLUDES) -lflags $(INCLUDES) -libs $(LIBS) -I src
OPTIONS = -use-ocamlfind -I src

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

# all: bin + tests + plugins

all: bin tests plugins

bin:
	ocamlbuild $(OPTIONS) $(TAGS) $(TARGETS_LIB) $(TARGETS_BIN)

tests:
	ocamlbuild $(OPTIONS) $(TAGS) $(TARGETS_TEST)

BUILD_PATH = $(PWD)/_build/src/
PLUGIN_OPTIONS = -I src/ -cflags -I,$(BUILD_PATH) -lflags -I,$(BUILD_PATH) -lib lib
plugins:
	ocamlbuild -use-ocamlfind $(TAGS) $(PLUGIN_OPTIONS) $(PLUGINS)

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

tags:
	otags $(IMPLEMENTATION_FILES) $(INTERFACE_FILES)

dot:
	for i in *.dot; do dot -Tsvg "$$i" > "$$( basename $$i .dot )".svg; done

.PHONY: all bin clean tags doc dot tests plugins

.PHONY: containers
