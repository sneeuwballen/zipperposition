
INTERFACE_FILES = $(shell find src -name '*.mli')
IMPLEMENTATION_FILES = $(shell find src -name '*.ml')
TARGETS_LIB = src/lib.cmxa src/lib.cma 
TARGETS = src/zipperposition.native tests/tests.native
LIBS = datalog,str,nums
#SUBMODULES = datalog sequence
SUBMODULES =
PWD = $(shell pwd)
#INCLUDES = -I,$(PWD)/datalog/_build,-I,$(PWD)/sequence/_build/
INCLUDES = -I,src
#OPTIONS = -cflags $(INCLUDES) -lflags $(INCLUDES) -libs $(LIBS) -I src
OPTIONS = -use-ocamlfind -libs $(LIBS) -I src -cflags $(INCLUDES) -lflags $(INCLUDES)
#OPTIONS_LIB = -I src -cflags $(INCLUDES) -lflags $(INCLUDES)
OPTIONS_LIB = -use-ocamlfind -I src -cflags $(INCLUDES) 

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

.PHONY: all profile clean tags doc tests dot

# libraries

datalog:
	make -C datalog

.PHONY: datalog
