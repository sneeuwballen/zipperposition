
- accept recursive functions in TIP input (so we know exactly what they look like)
  * → can allow for simple "smallcheck" in lemma generation
  * maybe also "smallcheck" for other clauses? probably not.
  * compile those functions into clauses by flattening, at the last moment
    (only for non-boolean functions, we know each case will be one clause)
  * boolean functions? how to do it?
    NOTE: compile to clauses, or rewrite rules?

  * [x] add full-def to untypedast + parser
  * [x] add "decl + set of rewrite rules" to statement
  * [ ] in Statement, allow a set of rewrite rules to be a definition?
  * [ ] proper translation of definitions in CNF
    + introduce datatype for boolean (`btrue|bfalse`) and lift most constructs
      to it. Means that we distinguish computable logic from classic logic
      (bool vs prop)....
      OR express `T.true/T.false` as a regular datatype in prelude?
    + special splitting rule on booleans (non-recursive datatype), to
      be sure to decide between btrue/bfalse.
      → refer to FOOL paper
    + handle conditional rewriting by adding secondary function
      `f x = g x if P` becomes `f x = f2 x P` and `f2 x true = g x`
    + handle matching on non-trivial exprs by secondary function
      `f x = match g x with C -> rhs` becomes `f x = f2 x (g x)`
      and `f2 x C = rhs`
  * ([ ] make rewrite rules conditional (with conjunction of atomic conditions))
  * ([ ] handle conditional rules in Rewrite (+ narrowing, where they turn to new lits)
        (applies iff the whole condition simplifies to true))

- use TIP input to run on all benchmarks normally (with rewriting
  instead of assertions, too)

- functional induction:
  * based on assumption that recursive functions terminate
  * build functional induction scheme(s) based on recursive def, might
    prove very useful for some problems

- generate all lemmas up to given depth
  * need powerful simplifications from the beginning (for smallchecking)

- change generalization technique:
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

- refine enum_types for
  `./zipperposition.native --dot /tmp/truc.dot -t 30 --show-lemmas examples/tla/fsm5.zf --debug 1`

## In Hold

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
