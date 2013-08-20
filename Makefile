# Main makefile for LogTK

NAME = logtk
VERSION=0.1

INTERFACE_FILES = $(shell find src -name '*.mli')
IMPLEMENTATION_FILES = $(shell find src -name '*.ml')
SUBMODULES = containers datalog
PACKAGES = 
CAML_OPTS = 
CAML_LIBS = str nums unix

# build targets
TARGETS_LIB = src/logtk.cmxa src/logtk.cma src/logtk.cmi
TARGETS_TEST = tests/run_tests.native

# output, ready to install
LIBS = $(addprefix _build/,$(TARGETS_LIB))
INSTALL = $(LIBS)
PWD = $(shell pwd)

OPTIONS = -use-ocamlfind

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

all: lib tests doc

lib:
	ocamlbuild $(OPTIONS) $(TARGETS_LIB)

tests:
	ocamlbuild $(OPTIONS) $(TARGETS_TEST)

doc:
	ocamlbuild $(OPTIONS) src/logtk.docdir/index.html
	cd src; find . -iname '*.ml{,i}' | xargs ocamlfind ocamldoc \
		-I ../_build/src -I ../_build/containers \
		-dot -o modules.dot
	cd src; find . -iname '*.ml{,i}' | xargs ocamlfind ocamldoc \
		-I ../_build/src -I ../_build/containers \
		-man -d man/

clean:
	ocamlbuild -clean

# install the main binary
install: lib 
	ocamlfind install $(NAME) META $(INSTALL)
	@echo done.

tags:
	otags $(IMPLEMENTATION_FILES) $(INTERFACE_FILES)

.PHONY: all lib tests doc clean install tags
