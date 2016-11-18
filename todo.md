

## Now

- lemma by generalization (if `t` occurs on both sides of ineq?)
  * see what isaplanner does

## Main

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

- refine enum_types for
  `./zipperposition.native --dot /tmp/truc.dot -t 30 --show-lemmas examples/tla/fsm5.zf --debug 1`

## To Fix

- type error  in `tip-benchmarks/isaplanner/prop_46.smt2`
- `examples/pelletier_problems/pb49.p` (wrong CNF? also, bad type inference on first axiom)
- `examples/by_case.p`  should be unsat, but arith fails

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
