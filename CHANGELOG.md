# Changelog

## 0.6

- do not depend on `CCError.t` arity
- require `bytes` to keep compatibility with < 4.02 (`String.init` too recent)
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
- KBO checks that weights are > 0
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
- unif.res_head
- N-ary unification now uses `Sequence` (simpler, more efficient)

## 0.5.1

- sort list of modules in doc/api_intro.txt
- code cleanup in `ScopedTerm.Seq`, less closures, simpler
- bugfix in `scopedTerm.DB.open_vars` (shift variables!)
- profiling information in CNF
- safety check in CNF: clauses must be closed (!)
- bugfix: Formula.is_closed
- fix printing of Type in TPTP
- add `PrologTerm.Syntactic` constructor (in place of `SimpleApp` which doesn't make much sense)
- cleanup: check i>=0 for bvar/var in ScopedTerm, share a common fresh_var generator
- simplified and factorized some code (share term containers)

## 0.5

- module `Sourced`; removed `Formula.sourced` and the likes
- changed printing of arith variables
- simplify interface for **De Bruijn** in ScopedTerm;
  more compact implementation using iterators
- safe API for type inference, using the error
  monad `CCError.t`` everywhere (also in `Util_tptp`)

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
    * arith_hook printer in `FOTerm`
- enable bin_annot
- printer hooks in formulas
- `Unif`: equality up to substitution implemented
- in `NPDTree`, do not capture Not_found that could be raised by the user callback
- lexicographic combinators in `Comparison`
- module `lib/IArray` for immutable arrays (previously used in Multiset)
- more abtract requirement for Clause index (feature vector...)
- cli flag to disable ordering generation in hysteresis
- hooks for `Cnf`

note: git log --no-merges previous_version..HEAD --pretty=%s
