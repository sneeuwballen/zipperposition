
INTERFACE_FILES = \
	terms.mli pp.mli foSubst.mli \
	orderings.mli foUtils.mli foUnif.mli index.mli superposition.mli \
	stats.mli paramod.mli nCicBlob.mli nCicProof.mli \
	nCicParamod.mli

IMPLEMENTATION_FILES = $(INTERFACE_FILES:%.mli=%.ml)

all:
	ocamlbuild -libs str,unix main.native

clean:
	ocamlbuild -clean

tags:
	ctags $(INTERFACE_FILES)

.PHONY: all clean tags
