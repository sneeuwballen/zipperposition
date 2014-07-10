# Changelog

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
