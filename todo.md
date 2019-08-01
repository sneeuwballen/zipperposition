# TODO

## Now

- release 1.5, with `dune` as main build system

- wip: proof checking for arith ℚ
  `./zipperposition.native --check -t 30 -o none --dot-llproof /tmp/truc.dot examples/GEG022=1_rat.p --debug.llproof 5`

- proof checking for arith ℤ
  `./zipperposition.native --check -t 30 -o none --dot-llproof /tmp/truc.dot examples/regression/gh_7.zf`

- refactor Monome by making it a functor over terms?

- arith:
  * understand why GEG rational problems are much harder
    (ordering? missing optim?)
    → self paramod with, say, transitivity of `<`

- FIX:
  `./zipperposition.native --check --dot /tmp/truc.dot --dot-sat -o none -t 30 examples/regression/ho_demod_partial_app.zf -d 5 |& less`
  completing `f=λx. t` into `f x=t` makes this problem unsolvable
  → keep both completed and original defs? (in Cnf)

- FIX:
  `./zipperposition.native --check --dot-llproof /tmp/foo.dot -o none -t 30 --dot /tmp/truc.dot examples/ho/sum_unin.zf --debug 5 --backtrace`
  pb is that rewriting under λ is not accounted for properly in proof
  checking because the instance of the rewrite rule is not closed.
  * possible solution: store (closed) context along with instantiation so
    we can have `λx y. f(x)+f(y)=f(y)+f(x)` instead of open terms `f(x)+f(y)=f(y)+f(x)`

- FIX: ordering issue in arith
  `./zipperposition.native --stats --dot-sat -o none -t 30 --dot /tmp/truc.dot --prelude ~/workspace/bset/bset.zf examples/Operators-B_translation-op1_2.p -p -t 300 --backtrace `

- FIX:
  bind twice some var?
  `./zipperposition.native --stats --dot-sat -o none -t 30 --dot /tmp/truc.dot examples/ho/find_set_inter.zf`

- proof checking
  * [x] update proof generation with renamings (or not), including rewrite
    steps and demod steps
  * [x] test this (without checking) on all TPTP; look for quick errors; merge into dev
  * ~~[ ] write `LLTerm.t` and basic functions (substitution/typing in particular)~~
  * [x] write simple CC based tableau (if possible, somehow incremental)
  * [x] conversion statement → formula
  * [x] make inference steps sth like
        `intros [x,y,z]; apply C1 [g(y),x+1]; apply C2[z,z]; tableau`
  * [x] skip some steps based on metadata (esa/arith) **for now**
  * [x] final summary on how many steps skipped/ok/fail
  * [x] debug on pure FO
  * [ ] store result of checking inside proof steps
  * [x] direct β reduction of llterms
      ~~ite/bool/β reduction rules in tableau~~
  * [ ] make rewriting under λ terms pass proof checking
  * [ ] proof checking for arith: FM / omega(?)/cooper
  * [ ] lazy equality exchange (case split on all equalities between arith terms?)
  * [ ] turn checking on by default

- write some examples based on HO patterns

- investigate useless arith inferences in
  `./zipperposition.native --stats --dot-sat -o none -t 30 --dot /tmp/truc.dot examples/verifast/foreach_remove_easy_pb.zf --induction-depth 0 --debug 1 | less`

- HO enumeration for fully applied function or predicate variables.
  should complement syntactic unif with constraints for HO.

- investigate very slow induction in
  `./zipperposition.native --stats --dot-sat -o none -t 30 --dot /tmp/truc.dot examples/verifast/foreach_remove_easy_pb.zf --induction-depth 0 --debug 1 | less`

## Misc

- in indexing, for `λx. f t1…tn` with `f` constant, perhaps still return `f`
  as head (with no sub)
  → no other head can work anyway

- [ ] use ocamlify for loading prelude files from `data/`? With special
    handling in `--prelude` for extensionless files (e.g. `--prelude set`)
  * [ ] option to print a given prelude file (`--print-prelude set`)

- [ ] in TPTP, detect *some* definitions (`definition, foo = bar`)
  with flag to disable it

- induction:
  * [ ] disable `gen-term` by default (too dangerous?)
        OR: refine notion of passive position (if definition patterns are deep),
          or totally disable it for arguments that are not inductive?
        see: `/zipperposition.native -p --print-lemmas --stats -o none -t 30 --dot /tmp/truc.dot examples/rewrite4_gen.zf`
  * [ ] find good ways of limiting the number of sub-inductions, and quick

- test prop:
  * [x] bring back few steps of saturation
  * [ ] how to test_prop on sth with skolem symbols
      (e.g. to eliminate `rev x=filter p x`)?
  * [ ] bring back a tiny amount of smallcheck to instantiate variables
    and realize that `forall l1 l2:list.  l1=l2` is absurd?

- [ ] rewrite meta-prover directly with `Term.t`(?) or something similar,
    using custom bottom-up solver (much simpler than calling datalog…?)
    or functorized datalog.
  * [ ] use it to detect instances of AC
  * [ ] rewrite system for group theory? also, ring theory?

- [ ]
  **PROOF CHECKER** (with intermediate internal format which separates
  instantiation and basic "ground" reasoning, perhaps)

- [ ] continue rat arith (fix pb 340, ordering, then var elim)

- [x] CLI option to hide types when printing terms (useful with lot of polymorphism)

- [] heavy penalty on the number of variables per clause (quadratic
  function n_vars → weight)

- penalty on high num of vars in hornet
  * heuristics in hornet (pick given ratio)
  * remove trivial non-horn clauses in hornet

- subsumption on unit horn clauses!
- maybe subsumption on other horn clauses, too

- [ ] put rewrite rules (and their source) in proofs

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

### Induction

* custom induction schema, with a toplevel command
  + structural induction on datatypes:
    `inductive (n:nat) := { (zero, {}), (succ(n'), {n'}) }`
  + induction on sets
    `inductive (S:set a) := { (S=empty, {}), (∃x S'. (S = S' ∪ {x} ∧ x∉S'), {S'}) }`
    corresponding to axioms:
    `forall S. S = empty xor ∃x S'. (S = S' ∪ {x} ∧ x∉S')`
    `[ P empty && (forall x S S'. x ∈ S ∧ S = S' ∪ {x} ∧ x∉S' ∧ P(S') => P(S)) ] => ∀S. P S.`
    NOTE: need to restrict its application, not needed for most use cases
    and will only slow things down
    → only when goal involves recursive function on sets?

  + inductive relations? if `R` transitive:
    `inductive (R(x,y)) := { (x=y, {}), (x ≠ y, {∃z. R(x,y) ∧ R(y,z)}) }`

* discussion
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
* [x] do induction on *simplified* formula (e.g. for HO functions)
* [x] notion of active position should also work for
      defined propositions (look into clause rules)
      + [x] factor the computation of positions out of `rewrite_term`
          and abstract it so it takes a list of LHS
      + [x] move this into `Statement`? propositions defined by clause rules
          with `atom t` as LHS should be as "defined" as term RW rules
      + [x] small check truly needs to use clause rules, too
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
* [ ] generalize a bit notion of ind_cst (to ground terms made of skolems)
* [ ] functional induction:
  + based on assumption that recursive functions terminate
    → ask for [wf] attribute?
    → maybe prove termination myself?
    → require that such functions are total! (not just warning)
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
* FIX
  `./zipperposition.native --print-lemmas --stats -o none -t 30 --dot /tmp/truc.dot tip-benchmarks/benchmarks/tip2015/list_return_1.smt2`
  (problem is, we should *generalize* `sk_f sk_x` before doing induction,
   or consider that a pure skolem term is an inductive constant,
   or consider that a skolem function returning an inductive type is an
   inductive constant)
* FIX
  `./zipperposition.native -p --print-lemmas --stats -o none -t 30 --dot /tmp/truc.dot tip-benchmarks/benchmarks/tip2015/list_elem.smt2`
  need to be able to pattern-match on boolean (also in TIP-parser)
* FIX
  `./zipperposition.native -p --print-lemmas --stats -o none -t 30 --dot /tmp/truc.dot tip-benchmarks/benchmarks/tip2015/tree_SwapAB.smt2`
  stack overflow because (CNF of neg of) lemma is too big?
* FIX
  `./zipperposition.native -p --print-lemmas --stats -o none -t 30 --dot /tmp/truc.dot tip-benchmarks/benchmarks/tip2015/int_add_inv_right.smt2`
  need to prove lemmas by regular neg+CNF if they don't trigger inductive proof
  → then, by elimination, we get another sub-proof that is inductive
* FIX
  `./zipperposition.native -p --print-lemmas --stats -o none -t 30 --dot /tmp/truc.dot tip-benchmarks/benchmarks/tip2015/list_nat_PairUnpair.smt2`
  problem is the destructor argument (need to define it automatically, with
  exactly one rewrite rule?)
* FIX:
  `./zipperposition.native --print-lemmas --stats -o none -t 30 --dot /tmp/truc.dot examples/ind/tree4.zf`
  does not pass anymore for some reason?
* translation between cstor-style and destructor-style?
  e.g. `x+y = if x=0 then y else (pred x) + (s y)`

- lemma guessing in induction:
  * [x] simple generalization of a variable with ≥ 2 occurrences in active pos,
        and ≥ 1 passive occurrences
  * [x] generalization of subterms that are not variables nor constructors,
        occurring at least twice in active positions.
  * [ ] track variable dependencies for generalized subterms, to avoid
        losing the (often crucial) relation between them
        and other terms containing the same variables.
        → need to also prove `t = t'` for every generalized term `t`
          and other term `t'` that shared ≥1 var with `t`
  * [ ] purification of composite terms occurring in passive position
  * [ ] anti-unification in sub-goal solving
      (e.g. `append a t1 != append a t2`, where `a` is a skolem
       → try to prove `t1!=t2` instead,
      if append is found to be left-injective by testing or lemma)
  * [ ] paramodulation of sub-goal with inductive hypothesis (try on `list7.zf`)?

- [x] make real inductive benchmarks
    (ok using tip-benchmarks)
  * [x] add `lemma` statement to tip-parser
  * [x] parse this in Zipperposition
  * [ ] use quickspec to generate lemmas on Isaplanner problems
  * [ ] run benchmarks (without induction, with induction, with quickspec lemmas)

- [x] lemma by generalization (if `t` occurs on both sides of ineq?)
  * see what isaplanner does
  * use "Aubin" paper (generalize exactly the subterms at reductive position),
    but this requires to have tighter control over rules/definitions first

- [ ] generate all lemmas up to given depth
  * need powerful simplifications from the beginning (for smallchecking)

### Datatypes

* [x] inference for acyclicity (not just simplification):
      given `C ∨ s = t`, look for σ such that `sσ = tσ` is absurd by acyclicity.
      Then infer `Cσ` from that.
      E.g. `s (f x) = s (s (f a))` would give `σ={x→a}`
      (do anti-unification with cstors only, then try to unify
       cstor-prefixed subterms on one side with the root on the other side)
* [ ] remove some specialized rules  (positive injectivity) and instead,
      generate rewrite rules during preprocessing
* [ ] hierarchic superposition for datatypes (with defined functions being part
    of the background)
  + [x] need corresponding TKBO with 2 levels
        (just replace KBO with it anyway, and build weight fun
        from constant classification)
  + [x] with TKBO implemented, removed the code that forces rpo6 to be
        used when induction is enabled, as well as constraint disabling
  + narrowing with defined symbols would ± correspond to E-unification on pure
    background literals
  + [ ] add purification inference (read carefully!)
        → do we want weak abstraction? would need 2 kinds of vars then
  + [ ] add "case split" rule for `t != u` where they are of a datatype.
        use a table for caching split for a given ground `t`.
        split looks like `t = cstor1(…) | … | t=cstor_k(…)` where
        each `…` is a list of fresh _parameters_ (i.e. possibly inductive
        skolems).
        → avatar should fire on that!
        Do **not** do case split on `α != β` where both are parameters
        of a **recursive** datatype (always possible to pick distinct
        values). For non-recursive datatypes we need to do it.
        → check on `examples/data/unit_…` problems
  + need a theory solver (msat + small SMT?) that deals with parameters
    → parameters are the way of dealing with exhaustiveness
* [ ] look into "superposition for fixed domains" more seriously
      (ask Weidenbach for more details?)

- rule similar to `fool_param` for for datatypes:
  `C[t]` where `t:nat` (strict subterm) is not a cstor term nor a variable
  would become `C[S x] ∨ t ≠ S x` and `C[0] ∨ t ≠ 0`
  * should be terminating (reduces the number of such strict subterms)
    but careful that with reduction you might find the same clause again,
    this must be an inference and not a simplification
  * is sound, and might be decreasing (check!).
    It does seem to work for fool.
  * enables more reductions…

### Rewriting

- conditional rewriting
  * [ ] parse `rewrite forall vars. ∧_i a_i => l = r`
  * [ ] same for clausal rewriting
  * [ ] handling by *inference* rule that unifies (rewrite & narrowing are the same)

    ```
    `∧_i a_i => l = r           C[l]    lσ=a
    ----------------------------------------
        ∧_i a_iσ => C[rσ]
    ```

    which is a form of superposition that is artificially restricted to
    rewriting `l` first

* [ ] for each rule, compile _fast_ pre-checks (e.g.
      matched term must have symbol `f` at arg position `i`) and use
      these before attempting call to `matching`
* [ ] in proof, put set of rewrite rules used in simplification steps,
      at least in full (non-compressed) version


### Higher-Order

- unification under constraints
  * [x] propose `unif_constraint: term -> term -> subst * (term*term) list`
  * [x] provide `is_syntactic_unifiable: term -> bool`
    (returns false on int,rat,HO terms)
  * [x] change `fold_terms` so it doesn't recurse under non-syntactic unifiable terms
  * [x] modify term indexing so it puts all non-syntactic unifiable terms in a box
  * [x] use `unif_constraint` in all inference rules, adding constraints
    as new literals
  * [x] disable purification
  * [ ] → evaluate perf impact on arith
  * [x] restore notion of "value" and fail constraints between distinct values
      (e.g. `0` and `1`)
  * [ ] some more advanced plugin scheme for failing fast on `a+1 = a`
  * [ ] two variations (optional arg) to avoid unifying `t` and `u:int` at
      root (useful for monome unif), or allow it (for subterms)
* [x] parse THF-0 and try on TPTP
    → some inductive problems contain higher-order
* [x] targets in `Makefile` for running on `^.p` problems
* [x] update `ctx.lost_completeness`
* [x] update CNF to flatten properly partially applied symbols
* [x] use Jasmin's KBO for higher-order terms
    + [ ] use the `ghd` function to increase precision
* [ ] fix RPO for partially applied terms
* [x] introduce builtin symbols S, K, and I with their definition
* [x] fix syntactic unification and indexing for higher-order terms
    (must consider them as right-parenthesed, be careful)
* [ ] HO purification (or rather, common framework for purification)
* [x] stream of pseudo-skolems `{c_1,c_2,…}` ordered by `i<j ⇒ c_i<c_j` as a
      block in precedence
* [x] on `f ≠ g` with functional types, where `{f,g} ∉ vars`,
      add `f c_i ≠ g c_i` for fresh pseudo-skolem `c_i`
      (do not do that for `F ≠ t` because it will refl-eq-destruct
       once `F` is not guarded anymore)
* [x] fool: rule `C ∨ p ≠ₒ q` => `{C ∨ p ∨ q, C ∨ ¬p ∨ ¬q}`
* [x] HO unification based on that (purify sub-terms of functional type,
    and deal with `a ≠ b` of functional types by successive steps
    of HO unification (structural rules = simplifications,
    choice points = inferences)
* [x] on `f = g`, infer `f x = g x`
* [ ] special rule instead of primitive enumeration: when predicate variable
    `P` occurs unguarded in a clause (only lits `P(t_i)` and `¬ P(u_j)`)
    perform variable elimination based on `P(x) = ∨_j x=u_j`
    with side-conditions `t_i≠u_j` (meaning replace by lits `t_i=u_j`)
  + conversion of λ-term to combinators on-the-fly for `P`
* [x] literal selection function:
  + do not select constraints with shielded vars
  + maybe *do* select prioritarily constraints with unshielded vars (to eliminate them)
* [ ] use S,K,I to λ-lift instead of introducing new symbol?
* [ ] debug: print terms with λ-expansion for clarity?
* optimize unification (shorter unifiers):
  + [ ] consider whether B,C combinators help make smaller unifiers
  + [ ] look at paper on combinators `ho/efficient_combinators_czajka_2016.pdf`
        for richer language of combinators (to find shorter unifiers)
* [ ] introduce one `fold` combinator per datatype, use these for
      HO-unification when arguments are of a datatype
      → need some hack for mutually recursive datatypes (perhaps take
        all arguments in each case, so we can properly write recursive
        definitions)

* [x] fix skolemization (in extensionality): need to depend on free vars

- [x] introduce λ terms
  * restore HO constraints (smallest lits, etc. but no eq-res)
  * check that superposition, etc. works inside λ terms
  * check orderings (compare λ between them, incomparable with others)
  * check indexing (under λ)
  * in unif, make `F t1…tn = u` delay as a (HO) constraint
  * implement full HO unif with depth limit, apply it on HO constraints

- [x] add special "ho constraint" literals
  * automatically used for purification of HO subterms
  * purification can occur in them for arithmetic terms, not FOOL nor HO
  * smaller than anything else(?)
  * always selected if they are not of the form `x =?= t` with `x` shielded,
    or flex/flex
    → in short, do flex/rigid prioritarily, but delay flex/flex or shielded
    variable purification
  * HO unification proceeds on these literals (perhaps by batch with
    a max number of steps), including syntactic steps → yields DNF.
    Put relatively high limit of steps?
  * no superposition/rewriting in them

- HO equality resolution (in addition to HO factoring and resolution)?
  **OR**: purify all HO terms into HO constraints, i.e. `F a ≠ G b` actually
  becomes `X₁ ≠ X₂ ∨ X₁ =?= F a ∨ X₂ =?= G b`, then normal FO eq-res,
  then `F a =?= G b` solved by HO unif
- [x] pattern unification
  * on the fly abstraction over ground arguments:
    `F a = t` becomes `λx. F x = λx. t[x/a]`
  * also allows for pattern HO-matching in rewrite rules

### Arith

- [x] simplification rule similar to `trivial_ineq` but for removing literals? E.g. to
  simplify `len a+len b < 0` from `len x ≥ 0`


### Misc

- prelude(s):
  * [ ] mechanism for loading .zf files into the source code (like oasis does)
  * [ ] `--prelude foo,bar` to load builtins
  * [ ] prelude for HO, with all the reifed operators as
        defined constants, e.g.
        `def forall_dot : pi a. (a->prop)->prop where
          forall p. forall_dot p = forall x. p x.`
  * [ ] prelude for set theory (`bset.zf`?)

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

- factor code between int arith and rat arith (and HO?)

- unification under constraints
  * [x] propose `unif_constraint: term -> term -> subst * (term*term) list`
  * [x] provide `is_syntactic_unifiable: term -> bool`
    (returns false on int,rat,HO terms)
  * [x] change `fold_terms` so it doesn't recurse under non-syntactic unifiable terms
  * [x] modify term indexing so it puts all non-syntactic unifiable terms in a box
  * [x] use `unif_constraint` in all inference rules, adding constraints
    as new literals
  * [x] disable purification
  * [ ] → evaluate perf impact on arith
  * [ ] restore notion of "value" and fail constraints between distinct values
      (e.g. `0` and `1`)
  * [x] two variations (optional arg) to avoid unifying `t` and `u:int` at
      root (useful for monome unif), or allow it (for subterms)


- heuristics:
  * [x] get back to a simple "pick-given ratio" with current sophisticated
    heap and a regular `Queue.t`?
    `/zipperposition.native -p --stats -o none -t 30 --dot /tmp/truc.dot examples/SEU140+2.p`
    should pass, doesn't look that hard (E does it)
    → check on all TPTP, though
  * [ ] penalize clauses with several positive lits?
  * [x] do not penalize deep induction (instead, restrict induction to
        positions that are active wrt definitions?)

- documentation
  * [x] large readme with usage tutorial
  * [x] detailed `.zf` syntax, including definitions and rewrite rules
        and operator priorities
  * [x] `-o none` and how to print graphviz proofs
  * [ ] a list of inference rules?

- [ ] better traces
  + [ ] rewriting steps should list set of rewrite rules used?
    make rewrite rules part of the proof graph (new case)
  + [x] the CNF part

- use `Type.mangle` for avatar skolems, too

## Done

- proof checking: implement eta-reduction
- when printing llproof, flag in red steps which failed (or red border)
- proof format with rewrite rules + CNF-introduced defs
- only do induction on active positions
  * [x] check that it fixes previous regression on `list10_easy.zf`, `nat2.zf`…)
  * [x] also check that sub-induction seems to hold water with smallcheck
        (i.e. does the ∀-quantified goal pass smallcheck?), otherwise
        do not do sub-induction.
        → will be useful after purification (approximation leads to too
          many inferences, some of which yield non-inductively true constraints,
          so we need to check constraints before solving them by induction)

- heuristic:
  * [x] per-inference penalty `int` field in clause. The higher, the
        worst. Each inference adds its own penalty, with
        a high penalty for sup/chaining from variable (explosion);
        replaces `age` in all queues but the `bfs` one
        → assess on GEG problems
        → also handicap risky inductions (these where all vars are generalized?)
        → also handicap very prolific arith rules

- [x] replace smallcheck by narrowing with a steps limit, not depth limit

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

- make `Test_prop` work with propositional rewrite rules
  → `./zipperposition.native --print-lemmas --stats -o none -t 30 --dot /tmp/truc.dot examples/ind/nat15.zf`
    check wrong lemmas and rate of success of smallcheck
  → `./zipperposition.native --print-lemmas --stats -o none -t 30 --dot /tmp/truc.dot examples/ind/nat21.zf --steps 1000`
    same, look at `small_check.fails`

- `./zipperposition.native --print-lemmas --stats -o none -t 30 --dot /tmp/truc.dot examples/ind/isaplanner_prop_12.smt2`
  HO purification gets in the way.
  Perhaps generalize `F ≠ f ∨ t[F]≠u[F]` into `F ≠ f ∨ t[F]=u[F]`
    instead of splitting? (keep all unif constraints as hypotheses)
  OR: before generalizing, perform destr-eq-res unconditionally?

## To Fix

- Arith:
  `./zipperposition.native --print-lemmas --stats -o none -t 30 --dot /tmp/truc.dot tptp/Problems/SWW/SWW616=2.p --no-avatar -p`
  performance drop with avatar…

- also need to find good way of indexing arith lits for subsumption
  (e.g. feature vectors with lower/upper bounds?)
  → tough, subsumption on these is hard

- induction: this should be faster, ideally (too many garbage lemmas)
  `./zipperposition.native -p --dot-sat --stats -o none -t 30 --dot /tmp/truc.dot examples/ind/list10_easy.zf --print-lemmas`

- `./zipperposition.native -p -o none -t 30 --dot /tmp/truc.dot tip-benchmarks/isaplanner/prop_66.smt2`
  better lemmas in induction (also do induction on normalized clauses)
  `./zipperposition.native -p -o none -t 30 --dot /tmp/truc.dot tip-benchmarks/isaplanner/prop_85.smt2`

  with speculative paramodulation/generalization
  `./zipperposition.native --print-lemmas --stats -o none -t 30 --dot /tmp/truc.dot examples/ind/nat15_def.zf`

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

