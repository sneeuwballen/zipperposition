
INTERFACE_FILES = $(wildcard *.mli)

IMPLEMENTATION_FILES = $(INTERFACE_FILES:%.mli=%.ml)

all:
	ocamlbuild -libs str,unix main.native

clean:
	ocamlbuild -clean

tags:
	ctags $(IMPLEMENTATION_FILES) $(INTERFACE_FILES)

.PHONY: all clean tags
