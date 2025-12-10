
ZIPP_TIMEOUT=30
MONO_TO=10

SYM_MONO_CAP=-1
SYM_MONO_MULT=-1
SYM_MONO_FLOOR=0

SYM_POLY_CAP=-1
SYM_POLY_MULT=-1
SYM_POLY_FLOOR=0

CLAUSE_MONO_CAP=-1
CLAUSE_MONO_MULT=-1
CLAUSE_MONO_FLOOR=0

CLAUSE_POLY_CAP=-1
CLAUSE_POLY_MULT=-1
CLAUSE_POLY_FLOOR=0

MONO_SUBST=10000
SUBST_CAP=-1


E_TIMEOUT=30
CLAUSE_MULT=-1
CLAUSE_CAP=2000

SUBST_ORDERING="age"

LOOP_NB=5
E_CALL_STEP=0

L_40_E_LIFT=(\
  -i tptp\
  -o none\
  --mode=ho-pragmatic \
  --max-inferences=4 --ho-max-app-projections=1 --ho-max-elims=0 --ho-max-rigid-imitations=2 --ho-max-identifications=0\
  --ho-unif-max-depth=2 --max-inferences=3\
  --boolean-reasoning=bool-hoist --bool-select=LO\
  --ext-rules=off --kbo-weight-fun=lambda-def-invfreqrank\
  --ho-prim-enum=none\
  --ho-unif-level=pragmatic-framework\
  -q "1|prefer-sos|conjecture-relative-var(1.01,s,f)"\
  -q "4|const|conjecture-relative-var(1.05,l,f)"\
  -q "1|prefer-processed|fifo"\
  -q "1|prefer-non-goals|conjecture-relative-var(1.02,l,f)"\
  -q "4|prefer-sos|pnrefined(3,2,3,2,2,1.5,2)"\
  --ho-elim-leibniz=1\
  --ho-fixpoint-decider=true --ho-pattern-decider=true --ho-solid-decider=true\
  --select=e-selection2 --solve-formulas=true --lambdasup=0\
  --e-encode-lambdas=keep\
  --presaturate=true --prec-gen-fun=invfreq --sine-trim-implications=true\
  --try-e="./eprover-ho" --tmp-dir="/tmp" \
  --e-call-step=$E_CALL_STEP --timeout=$ZIPP_TIMEOUT --e-timeout=$E_TIMEOUT\
  --sym-mono-ty-args="$SYM_MONO_CAP,$SYM_MONO_MULT,$SYM_MONO_FLOOR" \
  --sym-poly-ty-args="$SYM_POLY_CAP,$SYM_POLY_MULT,$SYM_POLY_FLOOR" \
  --clause-mono-ty-args="$CLAUSE_MONO_CAP,$CLAUSE_MONO_MULT,$CLAUSE_MONO_FLOOR" \
  --clause-poly-ty-args="$CLAUSE_POLY_CAP,$CLAUSE_POLY_MULT,$CLAUSE_POLY_FLOOR" \
  --monomorphising-subst-per-clause=$MONO_SUBST \
  --substitution-ordering=$SUBST_ORDERING \
  --e-max-derived=$CLAUSE_CAP --new-clauses-multiplier=$CLAUSE_MULT \
  --mono-loop=$LOOP_NB\
  --monomorphisation-timeout=$MONO_TO\
)

ZIPP_OPT=(\
  -i tptp\
  -o none\
  --try-e="./binaries/eprover-ho_2" --tmp-dir="/tmp" \
  --e-call-step=0 --timeout=30 --e-timeout=0\
  --e-call-point=1.0\
  --sym-mono-ty-args="-1,-1,999" \
  --sym-poly-ty-args="-1,-1,999" \
  --clause-mono-ty-args="-1,0,0" \
  --clause-poly-ty-args="500,0.5,50" \
  --monomorphising-subst-per-clause=5 \
  --substitution-ordering="age" \
  --e-max-derived=500 --new-clauses-multiplier=2 \
  --mono-loop=2\
  --monomorphisation-timeout=5\
)



#echo "running with ${????[@]}"
#find "$1" -name \*.p | xargs --max-args=1 --max-procs=1 timeout 30 ./zipperposition.exe ${L_40_C_S[@]}
#echo "done 40_c.s"
#
#find "$1" -name \*.p | xargs --max-args=1 --max-procs=1 timeout 30 ./zipperposition.exe ${L_35_FULL_UNIF[@]}
#echo "done 35_full_unif"

#find "$1" -name \*.p | xargs --max-args=1 --max-procs=1 timeout 30 ./zipperposition.exe ${L_40_C_IC[@]}
#echo "done 40_c_ic"

#find "$1" -name \*.p | xargs --max-args=1 --max-procs=1 timeout 30 ./zipperposition.exe ${L_40_NOFORMS[@]}
#echo "done 40_noforms"
#
#find "$1" -name \*.p | xargs --max-args=1 --max-procs=1 timeout 30 ./zipperposition.exe ${L_30_B_L[@]}
#echo "done 30_b.l"
#
#find "$1" -name \*.p | xargs --max-args=1 --max-procs=1 timeout 3000000 ./zipperposition.exe ${L_40_E_LIFT[@]}
find "$1" -name \*.p | xargs --max-args=1 --max-procs=1 timeout 3000000 ./zipperposition.exe ${ZIPP_OPT[@]}

