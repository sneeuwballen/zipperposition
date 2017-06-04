SHELL=/bin/bash

GOALS=$(shell cut goals.org -d '|' -f 2)
PROVER=zipperposition
TIMEOUT=2
PROVER_OPT=-t $(TIMEOUT) --induction-depth 0

all: easy hard

# Prove lemma foo from the definitions only
%_hard_pb.zf: goals.org
	@echo 'include "list.zf".' > $@
	@echo -n 'goal ' >> $@
	@grep " $* " goals.org | cut -d '|' -f 3 | sed "s/OR/||/g" >> $@

# Prove lemma foo from the definitions and the previous lemmas
%_easy_pb.zf: goals.org
	@echo 'include "list.zf".' > $@
	@sed "/$* /q" goals.org | sed '$$d' | cut -d '|' -f 3 | sed "s/OR/||/g" | sed 's/^/assert[sos] /' >> $@
	@echo -n 'goal ' >> $@
	@grep " $* " goals.org | cut -d '|' -f 3 | sed "s/OR/||/g" >> $@

# Call zipperposition and store its output in the target file foo_{easy|hard}_pb.zipper_proof
# Additionnally, foo_{easy|hard}_pb.time contains the time
# and foo_{easy|hard}_pb.status contains the status
# Echo the status
%.zipper_proof: %.zf
	@echo -n "$*: "
	@( time timeout $(TIMEOUT) $(PROVER) $(PROVER_OPT) $< > $@ ) |& cat > $*.time
	@cat $@ | grep -i status | cut -d ' ' -f 4 | tee $*.status
	@echo

# Try to prove all easy lemmas
easy:
	@for f in $(GOALS); do \
	$(MAKE) $${f}_easy_pb.zipper_proof; \
	done

# Try to prove all hard lemmas
hard:
	@for f in $(GOALS); do \
	$(MAKE) $${f}_hard_pb.zipper_proof; \
	done

# Remove all generated files
clean:
	rm -f *_pb.zf *.zipper_proof *.status *.time

.SECONDARY:
