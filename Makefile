
INTERFACE_FILES = $(shell find src -name '*.mli')
IMPLEMENTATION_FILES = $(shell find src -name '*.ml')
TARGETS = src/zipperposition.native tests/tests.native
LIBS = datalog,unix,str
SUBMODULES = datalog sequence
PWD = $(shell pwd)
INCLUDES = -I,$(PWD)/datalog/_build,-I,$(PWD)/sequence/_build/
OPTIONS = -cflags $(INCLUDES) -lflags $(INCLUDES) -libs $(LIBS) -I src

# switch compilation module
MODE ?= debug

all: $(MODE)

debug: $(SUBMODULES)
	ocamlbuild $(OPTIONS) -tag debug $(TARGETS)

prod: $(SUBMODULES)
	ocamlbuild $(OPTIONS) -tag noassert $(TARGETS)

profile: $(SUBMODULES)
	ocamlbuild $(OPTIONS) -tags debug,profile $(TARGETS)

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
