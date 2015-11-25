# OASIS_START
# DO NOT EDIT (digest: a3c674b4239234cbbe53afe090018954)

SETUP = ocaml setup.ml

build: setup.data
	$(SETUP) -build $(BUILDFLAGS)

doc: setup.data build
	$(SETUP) -doc $(DOCFLAGS)

test: setup.data build
	$(SETUP) -test $(TESTFLAGS)

all:
	$(SETUP) -all $(ALLFLAGS)

install: setup.data
	$(SETUP) -install $(INSTALLFLAGS)

uninstall: setup.data
	$(SETUP) -uninstall $(UNINSTALLFLAGS)

reinstall: setup.data
	$(SETUP) -reinstall $(REINSTALLFLAGS)

clean:
	$(SETUP) -clean $(CLEANFLAGS)

distclean:
	$(SETUP) -distclean $(DISTCLEANFLAGS)

setup.data:
	$(SETUP) -configure $(CONFIGUREFLAGS)

configure:
	$(SETUP) -configure $(CONFIGUREFLAGS)

.PHONY: build doc test all install uninstall reinstall clean distclean configure

# OASIS_STOP

rst_doc:
	@echo "build Sphinx documentation (into _build/doc)"
	sphinx-build doc _build/doc

open_doc: rst_doc
	firefox _build/doc/contents.html

push_doc: doc rst_doc
	rsync -tavu logtk.docdir/* cedeela.fr:~/simon/root/software/logtk/
	rsync -tavu _build/doc/* cedeela.fr:~/simon/root/software/logtk/rst/

test-all: build
	./run_tests.native
	./tests/quick/all.sh

VERSION=$(shell awk '/^Version:/ {print $$2}' _oasis)

update_next_tag:
	@echo "update version to $(VERSION)..."
	zsh -c 'sed -i "s/NEXT_VERSION/$(VERSION)/g" src/**/*.ml{,i}(.)'
	zsh -c 'sed -i "s/NEXT_RELEASE/$(VERSION)/g" src/**/*.ml{,i}(.)'

watch:
	while find src/ -print0 | xargs -0 inotifywait -e delete_self -e modify ; do \
		echo "============ at `date` ==========" ; \
		make ; \
	done

.PHONY: push_doc tags rst_doc open_doc test-all
