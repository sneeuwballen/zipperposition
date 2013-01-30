
INTERFACE_FILES = $(shell find src -name '*.mli')
IMPLEMENTATION_FILES = $(shell find src -name '*.ml')
TARGETS_LIB = src/lib.cmxa src/lib.cma 
TARGETS = src/zipperposition.native tests/tests.native
LIBS = datalog,sequence,unix,str
SUBMODULES = datalog sequence
PWD = $(shell pwd)
INCLUDES = -I,$(PWD)/datalog/_build,-I,$(PWD)/sequence/_build/
OPTIONS = -cflags $(INCLUDES) -lflags $(INCLUDES) -libs $(LIBS) -I src
OPTIONS_LIB = -I src -cflags $(INCLUDES) -lflags $(INCLUDES)

# switch compilation module
MODE ?= debug

all: $(MODE)

debug: $(SUBMODULES)
	ocamlbuild $(OPTIONS_LIB) -tag debug $(TARGETS_LIB)
	ocamlbuild $(OPTIONS) -tag debug $(TARGETS)

prod: $(SUBMODULES)
	ocamlbuild $(OPTIONS_LIB) -tag noassert $(TARGETS_LIB)
	ocamlbuild $(OPTIONS) -tag noassert $(TARGETS)

profile: $(SUBMODULES)
	ocamlbuild $(OPTIONS_LIB) -tag debug,profile $(TARGETS_LIB)
	ocamlbuild $(OPTIONS) -tag debug,profile $(TARGETS)

byte: $(SUBMODULES)
	ocamlbuild $(OPTIONS) -tags debug src/zipperposition.byte

doc:
	ocamlbuild $(OPTIONS) src/zipperposition.docdir/index.html

clean:
	ocamlbuild -clean

tags:
	ctags $(IMPLEMENTATION_FILES) $(INTERFACE_FILES)

.PHONY: all profile clean tags doc tests

# libraries

datalog:
	make -C datalog

sequence:
	make -C sequence

.PHONY: datalog
