
INTERFACE_FILES = $(shell find src -name '*.mli')
IMPLEMENTATION_FILES = $(shell find src -name '*.ml')
TARGETS = src/zipperposition.native tests/tests.native
LIBS = datalog,unix,str
SUBMODULES = datalog
PWD = $(shell pwd)
OPTIONS = -cflags -I,$(PWD)/datalog/_build/ -lflags -I,$(PWD)/datalog/_build/ -libs $(LIBS) -I src

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

.PHONY: datalog
