
INTERFACE_FILES = $(wildcard *.mli)

IMPLEMENTATION_FILES = $(INTERFACE_FILES:%.mli=%.ml)

all:
	ocamlbuild -libs str,unix -tag debug main.native
profile:
	ocamlbuild -libs str,unix -tags debug,profile main.native

clean:
	ocamlbuild -clean

tags:
	ctags $(IMPLEMENTATION_FILES) $(INTERFACE_FILES)

.PHONY: all clean tags
