
J?=3

all: build test-cached

DUNE_OPTS= -j $(J) --profile=release

build:
	@dune build @install $(DUNE_OPTS)

clean:
	@dune clean

doc:
	@dune build @doc

test:
	@dune runtest --no-buffer -f $(DUNE_OPTS)

install: build
	@dune install

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
	@echo "to run a particular test: ./tests/run_tests.sh -only-test <path>"

INTERFACE_FILES = $(shell find src -name '*.mli')
IMPLEMENTATION_FILES = $(shell find src -name '*.ml')
VERSION=$(shell awk '/^version:/ {print $$2}' zipperposition.opam)

update_next_tag:
	@echo "update version to $(VERSION)..."
	zsh -c 'sed -i "s/NEXT_VERSION/$(VERSION)/g" src/**/*.ml{,i}(.)'
	zsh -c 'sed -i "s/NEXT_RELEASE/$(VERSION)/g" src/**/*.ml{,i}(.)'

TEST_FILES=tests/ examples/
TEST_TOOL=logitest
TEST_OPTS?= -j $(J) --junit test.xml
DATE=$(shell date +%FT%H:%M)

check-test-tool:
	@if ` which $(TEST_TOOL) > /dev/null ` ; then true ; else echo "install $(TEST_TOOL)"; exit 1; fi

$(TEST_TOOL): check-test-tool
	$(TEST_TOOL) run -c ./tests/conf.toml $(TEST_OPTS) $(TEST_FILES) \
	  --summary snapshots/full-$(DATE).txt \
	  --csv snapshots/full-$(DATE).csv \

$(TEST_TOOL)-zipper:
	@mkdir -p snapshots
	$(TEST_TOOL) run -p zipperposition,zipperposition-check -c ./tests/conf.toml \
	  --summary snapshots/zipper-$(DATE).txt \
	  --csv snapshots/zipper-$(DATE).csv \
	  $(TEST_OPTS) $(TEST_FILES)

tip-benchmarks:
	git submodule update --init tip-benchmarks

docker-build:
	docker build -t zipperposition .

$(TEST_TOOL)-tip: check-test-tool tip-benchmarks
	@[ -d tip-benchmarks ] || (echo "missing tip-benchmarks/" && exit 1)
	@mkdir -p snapshots
	$(TEST_TOOL) run --meta=`git rev-parse HEAD` -c ./data/tip.toml \
	  --summary snapshots/tip-$(DATE).txt \
	  --csv snapshots/tip-$(DATE).csv \
	  $(TEST_OPTS)

# restricted version of $(TEST_TOOL)-tip
$(TEST_TOOL)-tip-isaplanner: check-test-tool tip-benchmarks
	@[ -d tip-benchmarks ] || (echo "missing tip-benchmarks/" && exit 1)
	@mkdir -p snapshots
	$(TEST_TOOL) run --meta=`git rev-parse HEAD` -c ./data/tip.toml \
	  --summary snapshots/tip-isa-$(DATE).txt \
	  --csv snapshots/tip-isa-$(DATE).csv \
	  $(TEST_OPTS) tip-benchmarks/benchmarks/isaplanner/

$(TEST_TOOL)-thf: check-test-tool
	@mkdir -p snapshots
	$(TEST_TOOL) run -c data/bench.toml --profile=thf  \
	  --summary snapshots/thf-$(DATE).txt \
	  --csv snapshots/thf-$(DATE).csv \
	  $(TEST_OPTS)

BENCH_DIR="bench-$(shell date -Iminutes)"
$(TEST_TOOL)-tptp:
	@echo "start benchmarks in ${BENCH_DIR}"
	@mkdir -p snapshots
	mkdir -p ${BENCH_DIR}
	cp zipperposition.native ${BENCH_DIR}/
	ln -s ../tptp/ ${BENCH_DIR}/tptp
	cp data/bench.toml ${BENCH_DIR}/conf.toml
	cd ${BENCH_DIR} && $(TEST_TOOL) run --meta=`git rev-parse HEAD` \
	  --summary ../snapshots/bench-$(DATE).txt \
	  --csv ../snapshots/bench-$(DATE).csv \
	  -c conf.toml $(TEST_OPTS)

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

