
INTERFACE_FILES = $(wildcard src/*.mli)

IMPLEMENTATION_FILES = $(INTERFACE_FILES:%.mli=%.ml)

all:
	cd src && ocamlbuild -libs str,unix -tag debug main.native

profile:
	cd src && ocamlbuild -libs str,unix -tags debug,profile main.native

doc:
	cd src && ocamlbuild main.docdir/index.html

clean:
	cd src && ocamlbuild -clean

tags:
	ctags $(IMPLEMENTATION_FILES) $(INTERFACE_FILES)

.PHONY: all clean tags
