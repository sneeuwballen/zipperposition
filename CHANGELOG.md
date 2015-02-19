# Changelog

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
