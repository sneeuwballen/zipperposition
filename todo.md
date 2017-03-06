# TODO

## Now

- heuristics:
  * [ ] get back to a simple "pick-given ratio" with current sophisticated
    heap and a regular `Queue.t`?
    `/zipperposition.native -p --stats -o none -t 30 --dot /tmp/truc.dot examples/SEU140+2.p`
    should pass, doesn't look that hard (E does it)
    → check on all TPTP, though
  * [ ] penalize clauses with several positive lits?
  * [ ] do not penalize deep induction (instead, restrict induction to
        positions that are active wrt definitions?)

- introduce builtin symbols S, K, and I, and use them to λ-lift?
  * [ ] HO unification based on that (purify sub-terms of functional type,
      and deal with `a != b` of functional types by successive steps
      of HO unification (structural rules = simplifications,
      choice points = inferences)
  * [ ] use Jasmin's KBO for higher-order terms
  * [ ] fix RPO for partially applied terms
  * [ ] fix unification and indexing for higher-order terms
      (must consider them as right-parenthesed, be careful)
  * [ ] parse THF-0 and try on TPTP
  → some inductive problems contain higher-order

- put rewrite rules (and their source) in proofs

- progress bar in hornet

## Hornet

- fix:
  * [ ] remove trivial clauses in splitting

- FO prover
  * [x] basic Horn superposition
  * [x] avatar splitting
  * [x] dismatching constraints for Horn superposition
  * [x] ⊥-grounding for clauses (map each clause to its grounding)
  * selection of one literal per clause that is ⊥-true (watch them during prop)
    → map ground lits to the corresponding set of clauses that contain it?
  * [x] eager saturation loop, with a bound on the number of unordered
        inferences for each clause (this bound = the one bound increased
        during iterative deepening).
        Ground Horn superposition and oriented rewriting are not bounded.
  * [x] full check for whether a HC already exists in saturation (avoid loops)
  * [x] some basic redundancy criteria
    + [x] subsumption on full horn clauses
    + [x] demodulation
  * [x] in demodulation, check *subsumption* of {label,constraint}
        (instead of just emptyness). Copy from variant?
        → important because many unit positive clauses will have a label
          or at least a (dismatching) constraint after splitting
  * [ ] deduplicate literals in general clauses
  * [ ] deduplicate literals in horn clauses' bodies
  * [x] depth limit for instantiation (too deep->instance goes in a waiting heap)
  * [x] avatar: on `[-p]`, add clause `[-p] => -[p]` (shortcut)
  * [ ] disequality/disunif constraints for `p ∨ x=y ← q, r, s`.
        it could still be a Horn clause: `p ← q, r, s | x!=y`?
        think about it
  * [ ] label merging rule, as DNF (set of labels, basically)?
        (each HC contains a list of clauses to re-add if it dies, the ones
         that were subsumed by label merging.
         OR: only part of a clause may die (hard!!))
        → might be too complicated to be worth it (try to have as few general
          clauses as possible anyway)

  `./hornet.native --debug 2 --dot /tmp/truc.dot -t 30 examples/pelletier_problems/pb34.zf`
  TODO: make more efficient (maybe do superposition with better heuristic
    and no depth limit, but step limit, with heavy penalty on
    clauses with high unordered_depth)
  also: 38

  subsumption / termination of saturation:
  `./hornet.native --debug 1 --stats --dot /tmp/truc.dot -t 30 tptp/Problems/RNG/RNG008-3.p`
  `./hornet.native --debug 1 --stats --dot /tmp/truc.dot -t 30 tptp/Problems/RNG/RNG008-4.p`

  `./hornet.native --debug 2 --dot /tmp/truc.dot -t 30 examples/RNG008-1.p` TODO: subsumption

- deduction modulo
  * [ ] move rewrite rules to core library
  * [ ] store rewrite rules in the defined (head) literal
  * [ ] adapt zipper code for horn clauses (what about rewriting on
        literals that does not yield a positive conjunction??)
  * [ ] improve efficiency heavily (e.g. on narrowing)̵
        → narrowing might be part of the "constraint" solving, not saturation
        → should eventually beat smbc on this (use SAT solver to guide)
        → think about mix of E-unif and HO-unif, driven by SAT

- hierarchic superposition
  * [ ] have a flag "abstraction" on `HVar.t`
  * [ ] update unification/matching algos so that they compute simple
        mgus (for `abstraction var =?= t`, modify occur-check so that
        it requires `t` to not contain `var` and to be a pure BG-term)
  * [ ] Horn clauses need a "constraint" part
  * [ ] have a basic theory of rationals (Fourier-Motzkin)
  * [ ] have a theory of datatypes
  * [ ] have a "define" rule for ground BG-sorted FG-terms (in particular,
        for datatypes, makes a good replacement to `x=0 ∨ x=s(_)`)

- induction
  * only on Horn goals (a goal=a Horn clause)
  * [ ] nested induction, same as in Zipperposition
  * [ ] strong induction:
    strengthening of `p(n) => q(n)` gives a new constant `n₀` and:
    + `p(n0)`, `¬ q(n₀)`
    + `p(x) => q(x) | x<n₀` where `<` is a BG theory symbol on all datatypes,
      interpreted as the structural ordering
  * [ ] explore whether abstraction (from hierarchic superposition) makes
        it easier to *simplify* candidate lemmas by generalization.
        Lemmas would be Horn clauses, so generalization is just removing some
        lits in body/constraint.

- linear integer arithmetic
  * [ ] Cooper for the BG constraints
  * [ ] a simple version of Zipperposition's code for the FG terms, as
        a powerful simplification/rewriting framework

- higher-order
  * [ ] do HO unification with a bound on the number of difficult choice points
        (imitation should be free, it's guessing that is severy restricted)
        → should be good enough for many problems
  * [ ] some form of bottom-up synthesis?

## Zipperposition

- make real inductive benchmarks:
  * [x] add `lemma` statement to tip-parser
  * [x] parse this in Zipperposition
  * [ ] use quickspec to generate lemmas on Isaplanner problems
  * [ ] run benchmarks (without induction, with induction, with quickspec lemmas)

- lemma by generalization (if `t` occurs on both sides of ineq?)
  * see what isaplanner does
  * use "Aubin" paper (generalize exactly the subterms at reductive position),
    but this requires to have tighter control over rules/definitions first

- documentation
  * [x] large readme with usage tutorial
  * [x] detailed `.zf` syntax, including definitions and rewrite rules
        and operator priorities
  * [x] `-o none` and how to print graphviz proofs
  * [ ] a list of inference rules?

- rule similar to `fool_param` for for datatypes:
  `C[t]` where `t:nat` (strict subterm) is not a cstor term nor a variable
  would become `C[S x] ∨ t ≠ S x` and `C[0] ∨ t ≠ 0`
  * should be terminating (reduces the number of such strict subterms)
    but careful that with reduction you might find the same clause again,
    this must be an inference and not a simplification
  * is sound, and might be decreasing (check!).
    It does seem to work for fool.
  * enables more reductions…

- [ ] better traces
  + [ ] rewriting steps should list set of rewrite rules used?
    make rewrite rules part of the proof graph (new case)
  + [x] the CNF part

- use `Type.mangle` for avatar skolems, too

- [ ] functional induction:
  * based on assumption that recursive functions terminate
  * build functional induction scheme(s) based on recursive def, might
    prove very useful for some problems

- [ ] generate all lemmas up to given depth
  * need powerful simplifications from the beginning (for smallchecking)

## To Fix

- `./zipperposition.native -p -o none -t 30 --dot /tmp/truc.dot examples/GEG024=1.p --debug 1`
  seems like heuristics are bad here (`--clause-queue bfs` works fine),
  need to penalize some deep series of arith inferences

  → also need to find good way of indexing these for subsumption
  (e.g. feature vectors with lower/upper bounds?)

- `./zipperposition.native -p --stats -o none --dot /tmp/truc.dot -t 30 examples/ind/list13.zf`
  need to avoid drowning in search space here

- `./zipperposition.native -p --stats -o none --dot /tmp/truc.dot -t 30 tip-benchmarks/isaplanner/prop_83.smt2 -t 120`
    trivial with `--clause-queue bfs`, need some fairness (using pick-given)

- rewriting:
  `./zipperposition.native -p --stats -o none --dot /tmp/truc.dot -t 30 tip-benchmarks/tip2015/sort_MSortBUPermutes.smt2`
  (maybe a rewrite rule that we try to use on paritally applied function)

## In Hold

- [ ] change generalization technique:
  * collect all "ground subterms of inductive type without cstor"
     (some are incompatible, if one if subterm of the other)
  * map them to distinct variables
    (maybe: map distinct occurrences of some of them to
     distinct variables. e.g. in "double"; more advanced though, check
     for absurd/trivial carefully)
     + Just do it for occurrences that block evaluation (Aubin 77),
       easy to compute the positions from function definitions. The other
       variables can remain "as is". Only do that if there are such positions
       on both sides of a `=`, or of a `=>` (i.e. in pos and neg lits)
  * reject combinations where all generalized terms are inductive csts,
    as those can be proved by mere nested induction
  * reject combinations where dependencies are not respected (i.e. terms
    that have same inductive cst(s) as subterms generalized as distinct
    variables; the dependency is lost)
    → actually this is useless?

- saturation based guessing of lemmas:
  * assume the goal is proved (with set-of-support
    composed of every axiom and def)
  * saturate for `k` steps (say, 100?)
  * among the clauses obtained this way, which result from the interaction
    between the goal and axioms, there should be interesting properties
    that will make good lemmas. If ⊥ is found, of course, report unsat.
    pick the `n` "best" such clauses that are small, general and non-trivial.
  NOTE:  need to have 2 separate superposition loops :/

Otter loop?
  * for forward demod/simp_reflect (all rewrite rules are used, even passive ones)
  * maybe for forward subsumption, too

## Long Term

- do the De Bruijn switch for rewriting/demod
  * convert `l=r` into De Bruijn indices (easy)
  * write matching `env:term db_env -> pattern:term -> term -> term db_env`
      (where only DB variables can be bound in the pattern)
  * write a small, simple, lightweight index for De Bruijn terms
  * re-write demod/rewriting to use this representation (carry a db_env
    along, as a kind of stack)

- basic support for integers in .zf ? parse numeric constants
  and a few infix operators

## Done

- [x] accept recursive functions in TIP input
  * → can allow for simple "smallcheck" in lemma generation
  * maybe also "smallcheck" for other clauses? probably not.
  * [x] compile those functions into clauses by flattening, at the last moment
    (only for non-boolean functions, we know each case will be one clause)
  * [x] boolean functions? how to do it?

  * [x] add full-def to untypedast + parser
  * [x] add "decl + set of rewrite rules" to statement
  * [x] in Statement, allow a set of rewrite rules to be a definition?
  * [x] proper translation of definitions in CNF
    + (introduce datatype for boolean (`btrue|bfalse`) and lift most constructs
      to it. Means that we distinguish computable logic from classic logic
      (bool vs prop)…)
      OR express `T.true/T.false` as a regular datatype in prelude?
    + [x] special splitting rule on booleans (non-recursive datatype), to
      be sure to decide between btrue/bfalse.
      → refer to FOOL paper
    + ([ ] handle conditional rewriting by adding secondary function
      `f x = g x if P` becomes `f x = f2 x P` and `f2 x true = g x`)
    + [x] handle matching on non-trivial exprs by secondary function
      `f x = match g x with C -> rhs` becomes `f x = f2 x (g x)`
      and `f2 x C = rhs`
  * [x] deal with higher-order application?
  * ([ ] make rewrite rules conditional (with conjunction of atomic conditions))
  * ([ ] handle conditional rules in Rewrite (+ narrowing, where they turn to new lits)
        (applies iff the whole condition simplifies to true))
