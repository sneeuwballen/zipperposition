# TODO

## Now

- fix:
  `./zipperposition.native -p --stats -o none --dot /tmp/truc.dot -t 30 examples/ind/nat2.zf`
  gives up and should not.
  → THEN, re-enable heuristic to penalize a bit deeply inductive clauses

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

- rewrite induction so it does everything by cut.
  * pros
    + it all becomes sound (assuming hidden induction principle): each cut
      is really the introduction of an instance of the induction principle
    + no nested induction anymore
    + allows to use `smallcheck` before trying a lemma
    + allows to refine a lemma by generalizing (Aubin 77) some specific
      subterms and some specific occurrences of variables, based on
      their position below defined symbols (in particular, for accumulator terms)
    + similar subgoals that would be distinct before (same goal, different
      skolems) are now the same lemma, thanks to the α-equiv checking
  * [x] from a clause `C` with inductive _skolems_ `{a,b,c}` we can generalize
        on these skolems without worrying,
        and try to prove `∀xyz. ¬C[x/a,y/b,z/c]` (but only do induction
        on variables that occur in active positions)
  * [x] remove trail literals for induction (and remove clause context,
        might have the higher-order induction principle for proof
        production though)
  * [x] generate fresh coverset every time; new inductive skolem constants
        really become branch dependent
        (no need to prevent branches from interfering every again!)
  * [x] call `small_check` on candidate inductive formulas;
        try simple generalizations backed by `small_check` before starting.
        → will be useful after purification (approximation leads to too
          many inferences, some of which yield non-inductively true constraints,
          so we need to check constraints before solving them by induction)
  * [x] only do induction on active positions
        → check that it fixes previous regression on `list10_easy.zf`, `nat2.zf`…)
  * [x] might be a bug in candidate lemmas regarding α-equiv checking
        (see on `nat2.zf`, should have only one lemma?)
  * [ ] **fix**: notion of active position should also work for
        defined propositions (look into clause rules)
        → factor the computation of positions out of `rewrite_term`
          and abstract it so it takes a list of LHS
        → move this into `Statement`? propositions defined by clause rules
          with `atom t` as LHS should be as "defined" as term RW rules
  * [ ] check if two variables are interchangeable (i.e. `{X→Y,Y→X}`
      gives same form). In this case don't do induction on the second one.
  * [x] do induction on multiple variables, **iff** they occur in active
        positions in the same subterm.
        + just combine the coversets for each variable
        + same as splitting, do union-find of `x,y` if there is an active subterm
          `f …x…y…` where both `x` and `y` are active
        + should subsume/replace the individual inductions (which are bound
          to fail since the subterms will not reduce because of one of
          their arg)
        + goes with generalization? If a non-var occurs in active position,
          it must be generalized? Maybe in 2 successive steps…
        + [ ] example: should prove transitivity of ≤
          `./zipperposition.native --print-lemmas --stats -o none -t 30 --dot /tmp/truc.dot examples/ind/nat21.zf`
        → might be sufficient for many cases where we used to use nested ind.
  * [ ] functional induction:
    + based on assumption that recursive functions terminate
    + build functional induction scheme(s) based on recursive def, might
        prove very useful for some problems.
    + applies for goals of the form `P[f(x…)]` with only variables in
      active/accumulator positions of `f`. In inductive hypothesis,
      use `P[f(skolems…)]`? or is it automatic with multi-var-induction?
  * [ ] use subsumption on candidate lemmas:
        + if a *proved* lemma subsumes the current candidate, then skip candidate
        + if an unknown lemma subsumes the current candidate, delay it;
          wait until the subsuming one is proved/disproved
        + **OR**,
            when a candidate lemma is subsumed by some active lemma,
            lock it and store it somewhere, waiting for one of the following
            conditions to happen:
          1. when a lemma is proved, delete candidate lemmas it subsumes
          2. when a lemma is disproved, unlock candidate lemmas that it
             subsumes and activate them (unless they are locked by other lemmas)
          → might even be in Avatar itself, as a generic mechanism!
        + when a lemma is proved, find other lemmas that are subsumed by it?
          or this might be automatic, but asserting [L2] if L1 proved and L1⇒L2
            might still help with the many clauses derived from L2
        + might need a signal `on_{dis_,}proved_lemma` for noticing
          that a lemma is now (dis,)proved by the SAT solver.
          → Hook into it to unlock/remove candidates subsumed by the lemma.
  * [ ] some clause constraints (e.g. `a+s b ≠ s (a+b)`) might deserve
        their own induction, because no other rule (not even E-unification)
        will be able to solve these.
        → Again, need very good and fast `small_check`…
  * [ ] when generalizing `f X a != f a X`, which currently yields
      the lemma `f X Y = f Y X`, instead we could "skolemize" `X`
      with a HO variable, obtaining `f (H Y) Y = f Y (H Y)`.
      see `examples/ind/nat6.zf` for a case where it can help
      (we need `H` because there already is a skolem out there).
  * [ ] strong induction?
      + use explicit `<|` subterm relation for the hypothesis
      + use special transitive rel saturation rules for `<|`
        (and nonstrict version `≤|`)
      + also use custom rules for subterm (using constructors)
        including simplification of `t <| cstor^N(t)`
        and corresponding unification inference rule
        → acyclicity just becomes the axiom `¬ (x <| x)` combined with above rules
      + no need for coverset anymore, just introduce skolems, but need
        (depth-limited) case-split on arbitrary constants/ground terms.
        → decouples case split and induction
      + when generalizing `¬P[a,b]` into `∀xy. P[x,y]`, when doing induction
        on `x`, might instead prove: `∀xy. y ≤| b ⇒ P[x,y]`? this way we
        can re-use hypothesis on `y` (and maybe `x`)?
      + multi-variable induction requires `<|` to work on tuples or multisets
        on both sides…
      + `./zipperposition.native --print-lemmas --stats -o none -t 30 --dot /tmp/truc.dot examples/ind/nat21.zf`

- theory of datatypes
  * [ ] inference for acyclicity (not just simplification):
        given `C ∨ s = t`, look for σ such that `sσ = tσ` is absurd by acyclicity.
        Then infer `Cσ` from that.
        E.g. `s (f x) = s (s (f a))` would give `σ={x→a}`
        (do anti-unification with cstors only, then try to unify
         cstor-prefixed subterms on one side with the root on the other side)
  * [ ] hierarchic superposition for datatypes (with defined functions being part
      of the background)
    + [ ] need corresponding TKBO with 2 levels
    + [ ] with TKBO implemented, removed the code that forces rpo6 to be
          used when induction is enabled
    + narrowing with defined symbols would ± correspond to E-unification on pure
      background literals
  * [ ] look into "superposition for fixed domains" more seriously
        (ask Weidenbach for more details?)

- only do induction on active positions
  * [x] check that it fixes previous regression on `list10_easy.zf`, `nat2.zf`…)
  * [x] also check that sub-induction seems to hold water with smallcheck
        (i.e. does the ∀-quantified goal pass smallcheck?), otherwise
        do not do sub-induction.
        → will be useful after purification (approximation leads to too
          many inferences, some of which yield non-inductively true constraints,
          so we need to check constraints before solving them by induction)

- purification:
  * [ ] think of purification at invariant/accumulator positions for
        defined terms of inductive types
        (steal code from arith)
  * [ ] de-purify when variable not guarded anymore
        → global notion of purification that interacts well with destr-eq-res?
  * [ ] purification of accumulator positions might be a HO variable
        applied to input parameters (to represent the dependency)?
        → need some HO unif!
  * [ ] should emulate speculative lemma guessing from Kapur et al.
      check on:
    `./zipperposition.native --print-lemmas --stats -o none -t 30 --dot /tmp/truc.dot examples/ind/nat15_def.zf`

- Higher-Order:
  * [ ] introduce builtin symbols S, K, and I
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
  * [ ] use S,K,I to λ-lift instead of introducing new symbol?
  * [ ] consider whether B,C combinators help make smaller unifiers
  * [ ] introduce one `fold` combinator per datatype, use these for
        HO-unification when arguments are of a datatype
        → need some hack for mutually recursive datatypes (perhaps take
          all arguments in each case, so we can properly write recursive
          definitions)

- rewriting:
  * [ ] for each rule, compile _fast_ pre-checks (e.g.
        matched term must have symbol `f` at arg position `i`) and use
        these before attempting call to `matching`
  * [ ] in proof, put set of rewrite rules used in simplification steps,
        at least in full (non-compressed) version

- heuristics:
  * [x] get back to a simple "pick-given ratio" with current sophisticated
    heap and a regular `Queue.t`?
    `/zipperposition.native -p --stats -o none -t 30 --dot /tmp/truc.dot examples/SEU140+2.p`
    should pass, doesn't look that hard (E does it)
    → check on all TPTP, though
  * [ ] penalize clauses with several positive lits?
  * [x] do not penalize deep induction (instead, restrict induction to
        positions that are active wrt definitions?)

- [x] make real inductive benchmarks
    (ok using tip-benchmarks)
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

- [ ] generate all lemmas up to given depth
  * need powerful simplifications from the beginning (for smallchecking)

## To Fix

- `./zipperposition.native -p -o none -t 30 --dot /tmp/truc.dot examples/GEG024=1.p --debug 1`
  seems like heuristics are bad here (`--clause-queue bfs` works fine),
  need to penalize some deep series of arith inferences **NO**

  → also need to find good way of indexing these for subsumption
  (e.g. feature vectors with lower/upper bounds?)

- `./zipperposition.native -p --stats -o none --dot /tmp/truc.dot -t 30 examples/ind/list13.zf`
  need to avoid drowning in search space here

- `./zipperposition.native -p --stats -o none --dot /tmp/truc.dot -t 30 tip-benchmarks/isaplanner/prop_83.smt2 -t 120`
    trivial with `--clause-queue bfs`, need some fairness (using pick-given)

- `./zipperposition.native -p -o none -t 30 --dot /tmp/truc.dot tip-benchmarks/isaplanner/prop_66.smt2`
  better lemmas in induction
  `./zipperposition.native -p -o none -t 30 --dot /tmp/truc.dot tip-benchmarks/isaplanner/prop_85.smt2`
  `./zipperposition.native -p -o none -t 30 --dot /tmp/truc.dot tip-benchmarks/isaplanner/prop_22.smt2`

  with speculative paramodulation
  `./zipperposition.native --print-lemmas --stats -o none -t 30 --dot /tmp/truc.dot examples/ind/nat15_def.zf`

- `./zipperposition.native -p --stats -o none -t 30 --dot /tmp/truc.dot tip-benchmarks/isaplanner/prop_03.smt2`
  too many induction inferences are made, should only do it on active positions
  since we have a rewrite system

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
