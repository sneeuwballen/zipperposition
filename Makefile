
INTERFACE_FILES = $(shell find src -name '*.mli')
IMPLEMENTATION_FILES = $(shell find src -name '*.ml')
PLUGIN_FILES = $(shell find plugins -name '*.ml')
TARGETS_LIB = src/lib.cmxa src/lib.cma 
TARGETS_BIN = src/zipperposition.native tests/tests.native
TARGETS_TEST = tests/tests.native
PLUGINS = $(PLUGIN_FILES:%.ml=%.cmxs)
LIBS = datalog
#SUBMODULES = datalog sequence
SUBMODULES = containers
PACKAGES = yojson zip num str dynlink

WITH_LIBS = $(addprefix -lib ,$(LIBS))
WITH_PACKAGES = $(addprefix -package ,$(PACKAGES))

PWD = $(shell pwd)
#INCLUDES = -I,$(PWD)/datalog/_build,-I,$(PWD)/sequence/_build/
INCLUDES = -I,src
#OPTIONS = -cflags $(INCLUDES) -lflags $(INCLUDES) -libs $(LIBS) -I src
OPTIONS = -use-ocamlfind $(WITH_PACKAGES) $(WITH_LIBS) -I src -cflags $(INCLUDES) -lflags $(INCLUDES)
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

all: native $(PLUGINS)

%.cmxs:
	ocamlbuild $(TAGS) $(OPTIONS_LIB) $@

native: $(SUBMODULES)
	ocamlbuild $(TAGS) $(OPTIONS_LIB) $(TARGETS_LIB)
	ocamlbuild $(TAGS) $(OPTIONS) $(TARGETS_BIN)

test: $(SUBMODULES)
	ocamlbuild $(TAGS) $(OPTIONS) $(TARGETS_TEST)

byte: $(SUBMODULES)
	ocamlbuild $(TAGS) $(OPTIONS) src/zipperposition.byte

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

.PHONY: all debug profile clean tags doc tests dot

# libraries

containers:
	make -C containers

.PHONY: containers
