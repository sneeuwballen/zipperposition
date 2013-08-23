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
TARGETS_LIB = logtk.cmxa logtk.cma logtk.cmi
TARGETS_TEST = run_tests.native

# output, ready to install
LIBS = $(addprefix _build/,$(TARGETS_LIB))
INSTALL = $(LIBS)
PWD = $(shell pwd)

OPTIONS = -use-ocamlfind -classic-display

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

all:
	ocaml setup.ml -all

bin:
	ocaml setup.ml -build

tests:
	ocaml setup.ml -test

doc:
	ocaml setup.ml -doc

clean:
	ocaml setup.ml -clean

# install the main binary
install: all
	ocaml setup.ml -install
	#ocamlfind install $(NAME) META $(INSTALL)

reinstall: all
	ocaml setup.ml -reinstall

uninstall:
	ocaml setup.ml -uninstall

tags:
	otags $(IMPLEMENTATION_FILES) $(INTERFACE_FILES)

.PHONY: all lib tests doc clean install reinstall uninstall tags
