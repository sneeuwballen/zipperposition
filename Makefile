
J?=3

all: build test-cached

DUNE_OPTS= -j $(J) --profile=release

build:
	@dune build @install $(DUNE_OPTS)

check:
	@dune build @check $(DUNE_OPTS)

clean:
	@dune clean

doc:
	@dune build @doc --profile=release

install: build
	@dune install

test: test-dune

test-dune:
	@dune runtest --no-buffer -f $(DUNE_OPTS)

uninstall:
	@ocamlfind remove zipperposition libzipperposition logtk || true

test-cached:
	@dune runtest --no-buffer $(DUNE_OPTS)
# ./tests/quick/all.sh # FIXME?

test-qcheck:
	@./tests/run_tests.sh test qcheck

test-long:
	@echo "run qcheck tests with --long"
	@QCHECK_LONG=1 ./tests/run_tests.sh test qcheck

test-unit:
	@./tests/run_tests.sh test units

test-list:
	@./tests/run_tests.sh list
	@echo "NOTE: "
	@echo "to run a particular test: ./tests/run_tests.sh -only-test <path>"

INTERFACE_FILES = $(shell find src -name '*.mli')
IMPLEMENTATION_FILES = $(shell find src -name '*.ml')
VERSION=$(shell awk '/^version:/ {print $$2}' zipperposition.opam)

update_next_tag:
	@echo "update version to $(VERSION)..."
	zsh -c 'sed -i "s/NEXT_VERSION/$(VERSION)/g" src/**/*.ml{,i}(.)'
	zsh -c 'sed -i "s/NEXT_RELEASE/$(VERSION)/g" src/**/*.ml{,i}(.)'

TEST_FILES=tests/ examples/
TEST_TOOL=benchpress
TEST_OPTS?= -j $(J) -c tests/$(TEST_TOOL).sexp --progress
DATE=$(shell date +%FT%H:%M)

snapshots:
	@mkdir -p snapshots
check-test-tool:
	@if ` which $(TEST_TOOL) > /dev/null ` ; then true ; else echo "install $(TEST_TOOL)"; exit 1; fi

$(TEST_TOOL)-local: check-test-tool snapshots
	$(TEST_TOOL) run $(TEST_OPTS) --task zip-local-test \
	  --summary snapshots/local-$(DATE).txt \
	  --csv snapshots/local-$(DATE).csv $(TEST_FILES)

.PHONY: docker-build
docker-build:
	docker build -t zipperposition .

TARBALL=zipperposition.tar.gz

package: clean
	rm $(TARBALL) || true
	oasis setup
	tar cavf $(TARBALL) Makefile pelletier_problems README.md src/ tests/ utils/

WATCH?=@all
watch:
	dune build $(WATCH) -w $(DUNE_OPTS)

ocp-indent:
	@which ocp-indent > /dev/null || { \
	  	echo 'ocp-indent not found; please run `opam install ocp-indent`'; \
		exit 1 ; \
	  }

reindent: ocp-indent
	@find src '(' -name '*.ml' -or -name '*.mli' ')' -print0 | xargs -0 echo "reindenting: "
	@find src '(' -name '*.ml' -or -name '*.mli' ')' -print0 | xargs -0 ocp-indent -i

gallery.svg:
	for i in gallery/*.dot ; do dot -Tsvg "$$i" > "gallery/`basename $${i} .dot`.svg" ; done

.PHONY: doc push_doc dot package tags test-all

