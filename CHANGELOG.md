# Changelog

## 1.3

- experimental proof checking with `--check` (and `--dot-llproof <file>`)
  does not work on all proofs, ignores arith and fails on demodulation
  under lambdas, for now.
- experimental HO branches from Alex Bentkamp (@benti), including advanced term orderings
  and variations on higher-order lambda-free superposition
- parser for a fragment of dedukti
- end-to-end proof output, starting from input statements
- large refactoring of `Proof`, with `Proof.result` being object-like
- get rid of dependency on `tip-parser`
- bugfix: prevent superposition on non-closed terms (from Geoff)
- Rename `--purify` to `--ho-purify`
- dont do sup at vars by default
- Replace kbo and rpo6 by their lambda-free counter-parts
  * Length-Lexicographic symbol status; Merge redundant term_to_head definitions
  * length-lexicographic comparison for lfhokbo
  * mandatory args for skolem constants (to not depend on choice)
- KBO with argument coefficients
- Blanchette's lambda-free higher order KBO (by @benti)
- Rename IDOrBuiltin to Head
- Blanchette's lambda-free higher-order RPO (by @benti)

## 1.2

- some HO support (with pattern unification)
- only ignore positive version of `x=y → …`, not other equational rules
- remove remnants of orphan criterion
- add `pp_in` signature in many modules
- demodulation under lambdas
- HO pattern unification
- add positive extensionality rule in HO
- add eta-reduction, enabled by default
- allow superposition to operate on non-closed terms
- better random generator for HO terms
- only perform primitive enum on clauses with low penalty
- primitive enumeration of predicate variables
- in FOOL, only extract closed terms to toplevel
- add `Lambda` module with WHNF and SNF
- proper skolemization in negative extensionality rule
- bugfixes for `Multiset`
- use logitest for tests
- remove `orient` tool
- full ZF printing, with proof output and proper commenting
- better stats and options for arith; cli option for `redundant_by_ineq`
- add polarized rewriting `rewrite foo => bar`
- fix bug in AC for non-FO terms (closes #13)
- introduce attribute for SOS (set of support)
- propagate attributes properly in CNF and type inference
- in induction, only keep inductive clusters(!)
- basic TPTP THF parser; update TPTP parser to support THF, remove ambiguities
- update literal ordering to have a basic approximation for rational lits
- add `ho-complete-eq` for completing positive equations
- block eq-resolution and destr-eq-res on shielded variables
- in induction, `x=y` puts x and y in same cluster
- branch using full HO unification
- branch using combinators for HO functions
- detect `.tptp` files as TPTP
- in `Cnf`, rewrite `f=λ x.t` into `∀x. f x=t`
- add `calculi/Higher_order` module; disable completeness if HO
- declare missing rule `C (proj-1 x)…(proj-n x) --> x` for cstors
- associate a rewrite rule to inductive type's projectors
- option for dumping sat solver logs into a file

## 1.1

- calculus for rational arith

- do not simplify too eagerly before trying induction
- add optional fuel limit on term rewriting
- disable orphan criterion by default
- remove age in clause queues; make FIFO queue simpler
- re-strenghten again eq-subsumption, by using anti-unification
- generalization on variables occurring both in active and passive pos in induction
- add `ArLiteral` to generate arbitrary literals
- disable `trivial-ineq` for arith, by default
- new clause queue, "almost-bfs"
- induction: simplify goals before doing cut on them
- apply exhaustiveness only for non-rec datatypes
- refactor `Test_prop` to use narrowing, instead of smallcheck
- in arith, remove completeness in case of `--no-arith`, too
- use a weight `α·ω+β` in (T)KBO precedence; use classify_cst for weights
- have some warnings in .zf files if identifiers are used undeclared
- normalize term `a=b`  into the corresponding literal
- do not do sub-inductions in clauses with positive lemma(s) in trail
- only do induction on active positions (or under uninterpreted syms)
- add pattern-matching and `if/then/else` to .zf native format
- add `Test_prop` in core library, with basic smallcheck
- add arithmetic to ZF parser
- bugfix in zipper: stornger purification avoid some spurious saturations
- in induction, only perform sub-induction in an already inductive clause
- rename `libzipperposition` back into `logtk`, fix some compat issues
- improvements in `Unif.unif_list_com` and IArray
- use new `FV_tree` in zipperposition
- add `FV_tree` structure, a new implem of feature vectors
- skolemization re-uses variables names, but lowercase
- add warn-error @8
- add dimacs dump for the SAT solver
- extract proof from SAT solver
- update to containers 1.0
- use a simpler `Hash` module
- use result everywhere, ditch variant-based error type
- add `forall (x y : ty). …` syntax (close #4 again)
- more concise syntax for `pi (a:type)(b:type).…` (close #4)
- collapse `Πx:type.… → type` into `type→…→type`
- lambda-lifting in `Cnf.flatten` to deal with anonymous functions
- fix type inference for higher-order applications (`var args`)
- basic parsing of arith operators and constants for TIP
- end-to-end proof tracing, simpler Statement, remove StatementSrc
- make `def-as-rewrite` true by default
- add `let` to core terms
- add `ite` and `match` in STerm and TypedSTerm
- add `Fool` calculus for dealing with boolean subterms
- more rules in `basic_simplification`
- remove everything about the meta-prover
- bugfix in `sat_solver` for evaluating 0-level lits
- allow boolean rewrite rules of the form `t` or `~ t`
- automatic re-generalization of non-induction variables in strengthening

## 1.0

- rewrite induction
- rewrite typechecking and CNF with new intermediate typed AST
- merge logtk into zipperposition again
- TIP parser
- remove hashconsed strings, use `ID` instead
- in TPTP, role "lemma" will be considered as a proper lemma
- share subproofs obtained from SAT solver
- better heuristic in ClauseQueue for deep inductive clauses
- add an `include` statement for ZF
- properly use Msat proofs in `Avatar.simplify_trail`
- make proof handling in `Sat_solver` transparent
- make `Dtree` bounded in depth
- add a new "AC" attribute in ZF, extend attribute system
- add `SClause`, unfunctorize `BBox,ProofStep,Trail`, use Msat proofs
- implement term narrowing (+ add some profiling)
- term and clause rewriting (deduction modulo)
- tests: use OUnit2
- Cnf: conversion of prop rewrite rules requires polarization
- optimize `injectivity_destruct{+,-}`: disregard type (dis)equations
- move almost everything from Zipperposition to Phases_impl
- introduce Phases, a monadic description of the steps taken by main
- make induction enabled by default
- add translation of inductive problems in .zf
- rename `type_check_tptp` into `type_check` (several input formats)
- conditional compilation of the prover itself (for debugging the library)
- add `warn` in Util, add colors in `Util.debug`
- update Hashcons, with global state, and alternative impl with Hashtbl
- move printing exception, factor code, rename `cnf_of_tptp`
- options for choosing the input format (zf/tptp)
- new parser and lexer for a ML-like format, `ZF`
- simpler, more efficient `Superposition.compare_literals_subsumption`
- change `FOTerm.Classic` so that `App` merges type and term arguments
- introduce TypedSTerm.Meta for destructive unification
- add a Binder module
- split Symbol into Symbol and Builtin
- distinction debug/debugf
- big change: use a pack again for core library
- refactor: rename `PrologTerm` into `STerm` (simple term)
- move src/base to src/core
- refactor: use `equal` and `compare`
- refactoring: change some types, fix warning, Format everywhere
- move `pelletier_problems` into `examples`; add `tests/` dir with script

## 0.8

### breaking
- breaking: remove `Type.tType`, makes no sense in the end
- breaking: `HOterm`: replace `TyAt` with `TyLift` (lifts a type to a term)
- breaking: add explicit `forall/exists` to `HOTerm`, remove rigid var case

### non-breaking
- fix bug in Type Inference
- more error reporting using `Printexc` and backtraces (requires `ocaml >= 4.01`)
- improve printers
- fix bug in `HOterm.open_at`
- convert `@τ` into `τ` in type inference, if required (application)
- `parser_ho`:
  * accept `(var:type)` as a term
  * fix ambiguities, parse `@a` (type lifted to term)
  * now lifts types to terms
  * fix parser so it handles forall/exists and has no ambiguity
- rename some constructors (prefixed with 'Logtk' because of overkill sed)
- bugfix in builtin.theory
- meta-prover: remove rigidification, use explicit quantifiers, udpdate builtin.theory
- meta-prover: use sections for debug, forbid rigid vars in axioms/theories
- add function in meta-prover encoding
- update vim syntax for theory files
- add a few functions to `HOTerm`
- printer in meta/encoding
- enable logtk.meta by default

## 0.7

- breaking: remove array utils from `LogktUtil`, use `CCArray` instead
- breaking: remove list utils from `LogktUtil`, use `CCList` instead
- vim support for theory files
- fix bug in `solving/Lpo` (fresh names collision)
- fix logtk.solving, using Msat rather than (patched) aez
- `Skolem.clear_skolem_cache`
- `Util.debugf` for Format-based debugging
- small fixes post-0.6.2

## 0.6.2

- fix the list of modules in API doc
- fix in `Precedence`: use symbol equality
- add general info in two cases of `parsers.ast_tptp`: `TypeDecl` and `NewType`
- fix quick tests
- more recent `opam` file
- `Skolem`: instantiation functions
- better printing of types (de Bruijn) in foterms
- use `Sequence` rather than `CCSequence`

## 0.6.1

#### breaking

- remove `-pack` for `Logtk`, renamed modules from
  `<Foo>` to `Logtk<Foo>` (to use 4.02s module aliasing)
- remove useless `-def-limit` argument to the CNF
- use `-no-alias-deps` when compiling

#### non-breaking

- fix `parsers/Util_tptp.find_file` behavior w.r.t `$TPTP` env variable
- do not rename atomic formulas in `CNF` (close #1)
- `FeatureVector.{iter,fold}` implemented
- flush `stdout` on debug
- add `Options.mk_global_opts`, deprecating `Options.global_opts`
- section mechanism for `Util.debug` (in `Util.Section`) with inheritance
- bugfix in `Precedence.Constr.invfreq`
- update arbitrary instances to use `qcheck-0.3`'s `fix_fuel` combinator
- `PartialOrder` (and `Precedence`) give more details when the ordering is not total
- new functions in the precedence
- new modules `Precedence_intf`, `Ordering_intf`, `PartialOrder_intf`

## 0.6

- do not depend on `CCError.t` arity
- require `bytes` to keep compatibility with `< 4.02` (`String.init` too recent)
- `Skolem`: at creation, now possible to specify prefix for Tseitin atoms
- `Util`: removed a String.create deprecation warning
- `FeatureVector.Make.retrieve_alpha_equiv`
- opam file (for easy developement version)

## 0.5.4

- better CNF (with more accurate criterion to choose whether to rename formulas)
- `Util.set_{memory,time}_limit`
- remove old files

## 0.5.3

- `FOTerm.weight`
- `Type.depth`
- KBO checks that weights are `> 0`
- use a flag in `Formula.simplify`, for memoization; makes CNF fast again on some examples
- expose `ScopedTerm.flags`
- `Precedence`: make it possible to choose/change the weight fun
- updated README to favor opam

## 0.5.2

- demo program `resolution1.ml`
- oasis: `--enable-demo` flag
- arithmetic binary predicates are polymorphic (`$sum`, etc)
- target in Makefile to update `@next_release` tags
- refactor:
    - unification now works with two (optional) DB environments in which bound variables of both terms live.
    - matching: parameter `?allow_open` is now used properly in rewriting and indexing
- `unif.res_head`
- N-ary unification now uses `Sequence` (simpler, more efficient)

## 0.5.1

- sort list of modules in `doc/api_intro.txt`
- code cleanup in `ScopedTerm.Seq`, less closures, simpler
- bugfix in `scopedTerm.DB.open_vars` (shift variables!)
- profiling information in CNF
- safety check in CNF: clauses must be closed (!)
- bugfix: `Formula.is_closed`
- fix printing of Type in TPTP
- add `PrologTerm.Syntactic` constructor (in place of `SimpleApp` which doesn't make much sense)
- cleanup: check i>=0 for bvar/var in ScopedTerm, share a common `fresh_var generator`
- simplified and factorized some code (share term containers)

## 0.5

- module `Sourced`; removed `Formula.sourced` and the likes
- changed printing of arith variables
- simplify interface for **De Bruijn** in ScopedTerm;
  more compact implementation using iterators
- safe API for type inference, using the error
  monad `CCError.t` everywhere (also in `Util_tptp`)

## 0.4

- cleanup:
    * remove submodules, `lib/Monad`, and many symlinks
    * replace `Monad.Err` with `CCError`
- explicit dependency of logtk on `sequence` and `containers`
- `logtk_parsers` depends on menhir as a build tool
- fix issues with build system (`solving` flag)
- substitution membership for views
- changed hash function
- use lib `benchmark` rather than `bench`
- use the new `CCHash` api for hash functions that take and return `int64` values
- add `Signature.is_empty`
- profiling annotations
- printing of `NPDtree` into dot
- use `CCList` and `CCKList` (in unification)
- allow to protect some variables in `Unif.Unary.match_same_scope`
- perf improvement: useless buffer allocation in `Util.debug`
- `Multiset`:
    * printer, qtest, and new comparison function
    * API change, now a functor dealing with very large multiplicities
        of elements using `Zarith`
- `ForallTy` constructor
- function `Comparison.dominates`
- bugfixes:
    * do not rename variables for skolemized formulas (otherwise variables wont match)
    * `Formula.simplify`
    * `Unif.Unary.eq`
    * `Comparison.@>>`
    * `FOTerm.head`
    * `arith_hook` printer in `FOTerm`
- enable `bin_annot`
- printer hooks in formulas
- `Unif`: equality up to substitution implemented
- in `NPDTree`, do not capture `Not_found` that could be raised by the user callback
- lexicographic combinators in `Comparison`
- module `lib/IArray` for immutable arrays (previously used in Multiset)
- more abtract requirement for Clause index (feature vector...)
- cli flag to disable ordering generation in hysteresis
- hooks for `Cnf`

note: git log --no-merges previous_version..HEAD --pretty=%s
