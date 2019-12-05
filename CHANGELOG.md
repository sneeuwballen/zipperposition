# Changelog

## 1.6

- style: reindent everything
- all parameters are converted to flex state parameters
- Jensen-Pietrzykowski Unification
- complete variables one-by-one in complete_eq_args (ArgCong)
- support more cases in THF parser
- stream queue for enumerating unifiers


- migrate vim-zf to zipperposition

reverted changes to streamQueue
fixed the problem with favorizing the no oracle procedures
added skip multiplier
fix generation of `const.ml`
chore: fix opam file wrt missing test-dep for logtk, jobs, conflicts
prepare for 1.5.1
chore: update travis
fix: compilation error in simplex
chore: update dockerfile
fix compilation issues
backport fixes so it compiles
some fixes to pattern+fixpoint
problems seem resolved
very strange behavior of deciders
fixing solid part
more
lazy fixpoint
fix to substitution being applied indefinitely in fixpoint unif
disabled solid unif for poly
more
more
fixes
added fixpoints
fixpoint init
cleaned up pattern unifiaction
various polymorphism fixes
name change
many polymorphism fixes
more
fingerprints + unification
fixed polymorphic TST conversion
fixes to cnf
fixed cnf bug
final fixes to unif
subset elimination
unif fix
added merge
add unif_queue
fixes to the full version
added huet-style projection to both algorithms and removed imitations
new options
final bugfix
further bugfixes
delegated support for unsupported literals back to original subsumption
fixed intersection bug
few more bugfixes
added solid subsumption option;
added option for solid subsumption
added interface file for solid subsumption
first version with all the ingredients
added skeleton of matching procedure
normalization of clauses
refactoring of code that grounds literals
added datatypes constructors covers and interesction functions for solid subsumption;
added forgotten params
merge, max_inferences still not there
added the profiler
fixed the problem with introducing unnecessary variables
lazified internal creation of term objects
added solidification limits
fix to pattern unif incompletness
fixed solid decider incompleteness bug
fixed the bug with covering
fixes from homepc
fix to solidunif
still some errors
version that seems to work
added option for including/excluding deciders
added assert for solid
fixed unit tests
added more var checks + solidification
added a function for computing Xs = Xt
added finitary deciders, take two
allowed finitary deciders
added mli file
in principle, should compute mgus now
added more supporting funcitons, writing unifier now
new params
added all auxiliary functions
added function for covering the rigid skeleton
added solid unif files
fix to binding orders
fixes to equality between functions problem
various fixes to bugs
fixes to projection + imitation mess
more
there is a problem with not counting imitations of constants -- investigate
support for smart elim + no increments to imitation
removed prints
various bugfixes to unif framework
introduced the notion of oracle composer -- should probably only stay in the implementation
fixed the whnf bug
fixed the bug with quantifier encoding
some fixes to binary flags
added support for imits
added forgotten files
more
skeleton for first framework pragmatic version
bugfix for decomposing AppBuiltin + fix for a bug where extra step was wrongly introduced (elim in different heads)
more
fixed the bug that caused the endless loop
fixed some problems
fixed some problems
chore(travis): enable 4.08
chore: add more profiling probes
more
compiles, but either has bugs or is super slow
added cmd line option
instantiated fully
jpfull
instantiation of framework for JP
allows inserting nones
first version of the framework
preliminaries
fix
doc: small tweak to readme
fixed problem with partial application of & |
fixes
fix: scope issue in local open
fix compile errors
more
super ugly fix with lists
new version, think there are still some bugs
added weight to iters
added caching of weight to innerterm
implemented caching of maximal literals;
fix
added ho_weight function for pdt caching
many fixes
fixed with alpha renaming
more
more
added mechanism for reusing the definitions
added alpha renamer for TST
chore: move to sneeuwballen organization
fixed fp indices
supporting quants
more
added quantifier instantiation in full mode
added negative choice triggers
fix for fp indexing
improved fp indexing
fixed parse error with new clause queues
new heuristics
added new kbo weight creation schemes
prepared for symbol, depth combination
fixed incomplete simplification
fix for the lambda lifting equality
buggy
possibly better proof reconstruction for lifted lambdas
added rectified version of lambda lifting
more
more
more fixes
more
added new choice treatment
more
more
added boolean normalization rule
turned off buggy unification
fixed fingerprint issue
more
complicated test case passed
bugfixes
added lambda lifting option
actually started declaring new defs
lambda lifting compiles
new selection + disabling superposition from pure variable literals (x=y)"
more
even more heruistics
added a lot of new heuristics
more
more priority functions
more proof markings
added new priority functions
bugfixes
it seems that different queues work
added multiple clause evaluation queues
more bugfixes
reverted back lfho unif
more bugfixes
fixed fp bug
improvements to fingerprint indexing
more fixes to FV tree indexing
fixed the problems with HO and feature vectors
fixed the problem with choosing clauses from queues
more
more
new ho_weight clause weight function
added some new selection functions
changed the way proof depth is calculated
marked all the higher-order parts of the proofs
refactoring of proof depth
fixed the rewriting bug and maybe sped up beta normalization
more
rewriting fixed
tested
added inference
added cmd line option
added proof creation
added function for removal of leibniz equality
rewrite before cnf does not work
cleanup
version of rewriting before cnf that compiles
more
moreg
first proof by e
many bugfixes
made context mandatory fixed the bug with skolem recreation
more
made zipperposititon register new skolems
many bugfixes
more
first version of E translation module
more
added option of better handling of decidable fragments
implemented new selection function and goal distance penalty for clause evaluation
changes to clause queues
added e's selection function
cleanup app_encode
more
more
removed out
starexec version
improved performance of unification
added grounding before subst
added better support for conversion
work pc
more
chore: stricter dune flags
fixes
added support for interpreting boolean functions as forall or forall ~
removed out
added clause normalization
fixes to triggered instantiation
more
cleaned up detecting cnfs
fix to eq factoring
remove mandatory args completely
app encode after cnf
more bugfixes
fixed quantifier head bugs
Completed eager boolean case splitting by adding post-CNF phase for handling terms created by Skolemization and subexpression naming.
added option to keep parents for boolean casing
fixed the problem with set557
still not the max, investigate why
more
fixed the selection restriction that was making us _very_ incomplete
check for arithmetic in lambda-free filter
better errors in app_encode
bugfix for lambda-free problem finder
more fixes to treatment of db quants
fixed various unification bugs
more fixes to ext_dec, still work to do
fix for not-expanded vars
more rigourous checks as to when to perform lfho unification
now compiles
conversion of quants
converting back and forth from the deBruijn indices
fixed some things with primitive enumeration
added better support for prim enum caching
refactoring of cannonization
registered cnf_otf as simplification
removal of complementary literals
fix to unif bug
fix to the various bugs with full formula subterms
portfolio fixed to work with python2 and StarExec
portfolio
fixed error where I reversed if and else
removed dead code
fixes
added indexed support for ext_dec
added indexing
refactoring of ext_dec, preparation for indexing
many fixes to negcongfun and unification
more conservative application of inner unification algorithms + lower inferences count for competitive mode
fixed the bug with renaming of equalities
fixed the issue with agressiveness of removing removing structure of and
fixes with eqres and eqfact
more
more
fixed bugs with injectivity recognizing
depth for the pattern unification
fixed wrong assertion
further improved unifciation
fixed unif bug
solved bug with ordering
fixes of boolean operators
very important update with quantified variables
added an important TODO
bugfixes
Added strategy to eager boolean case distiction which moves subterms as little as possible.
more simplification
option to add choice clause
save substitution for the trigger instantiation
better treatment of choice
still some bugs :(
added new simplifications
fixes
more options for primitive enumeration
better pragmatic primitive enumeration
added cmd line option
added choice and injectivity treatment
added smart instantiation of predicate variables
bugfix
build fix
added it to inferences
added ext decompose factoring
added support for ext eqres if all the subterms are the same
added support for ext_decompose in all subterms
set up competitive parameters properly
removed max depth penalty
fixed polymorphic ext decomposition
fixed nested equalities
unification fix
more
fixed free vars calculations
fixed treatment of bound vars
added ext_eqres_decompose
added performed inferences heuristic
avatar fixes
fixed competitive mode
solved syo304\^5
prune arg fix
added stat and profiling
removed prints
added options
made the driver for new extensionality decompose rule
more assertions fixed
fixed the leaked negative variables bug
fixed fiding max vars
more
normalization of vars
wrong assertion fix
fixed assertions
fixed the bug with negative propositions
local changes
possible fixes
further changes to prop removal
returned boolean reasoning
Fixing typos
added better treatment of equation polarity
fixed problems with clause queue evaluation
simplification of equalities between bools
more
fix for wrongly interpreted nested equations
added clause simplifier
added normalization function
moved  quantifier renaming option to Booleans.ml"
Added transformation to take boolean terms out before clausification.
closing clauses before cnf
rewriting fixes
eta reduction in definitions
many fixes, but there still might be a problem with renaming quantifiers
disabled superposition into/from variables bound by internal quantifier
more
proof making fixed
removed print
unified skolem context
added otf clausification
conversion rules
added conversion rules
more
monomorphic version of unification algorihtm that should work better;
fluidsup option
fixed aggressive renaming of quantifiers
removed the print statement
added type declarations for newly introduced symbols
Quantifier naming
new modes
single step simpl
fix: re-enable warning 50 and fix it
bool cases as a simplification rule
(in)equations between boolean atoms are no longer rewritten into (negative)equivalences
Case negative equations as positive too
added option fr selection of bool-subterm-seleciton
simplification rule for trivial (in)equalities
Better names
Top level detection in bool_cases
added support for demodulation with builtins
added support for restricting elim rules
Computational boolean axioms and handling of AppBuiltin.Eq and the like
fixed bug influencing the completeness of complete mode
add proof tags
cnf change + getting support for some HO inferences
superposition at variables
eta_reduce: small performance bugfix + tests
prop completely removed, first test case passed
removal of props;
bugfix: Flatten App of AppBuiltin
removed unif asserts
Command line option for boolean axioms.
before submitting to starexec
simplified unification
removed print messages
turned complex unifciation off
prefer LFHO unification;
unification support for Builtins
access test
new and
added and and or
fix: prevent warning 55 from blocking compilation
added cmd line option for supporting booleans
initial commit for boolean support with axioms
bugfix demod variable clashes
removed some debug;
3 projections
fixes to lambdasup
all-covers bug with builtins with no args fixed
more
disabled dupsup for polymorphic terms
remove mandatory args
removed superflous printing
bug with all-covers arg pruning and polymorphism seems to be resolved
fixed some peculiarity with lambdasup and polymorphism
fix for eta-short polymorphism
more
messages
added support for lambdasup with limit
fixes
FluidSup for deep variables
fix for rewriting
Adapt to new definiton of fluid terms
fix global option for eta conversion
lambdasup restricted
global option for eta expand/reduce
remove old implementation of eta-reduce
build fix
lambdasup fix
lambdasup fix
fixed ext_pos_general to work with mandatory args
extpos general
resolved bug with extpos
fixes
more
optimization
changed wrong proof step name
fixes
added cmd line option
added extpos on non-unit clauses
added neg ext as simplification rule
changed name of lambda-demod-ext to lambda-demod
added support for removal under lambdas -- the only important th8ing is that we do not refer directly to the bound vars
4 tests passed
more fixes
some fixes, still seems not to work
added the option for prune_arg_fun
first version of the rule that compiles
unit tests pass
some fixes
added function for max cover
fixes in cover function
tested function that covers
implemented function that covers terms
seems to work
commented out printout
dupsup variable issue seems resolved
seems dupsup does not work
more type fixes
type fixes
implemented dupsup
implemeted dupsup passive (into)
fixed negcongfun and negext
added NegExt
fixes
added rule for negcongfun
implemented negcongfun
decoupled rewriting into arguments and into bodies of lambdas
demodext fixed!
lambda and supext changed
chore: use `qcheck-core` rather than older full `qcheck`
tests pass
fixed warnings
more fixes
fix: do not use recent `float_of_string_opt`
fix: use existing ID Map
fix(demo): update signature
refactor: reuse existing intmap
fix unused var warnings
fix bug introduced by merge
replace Sequence by Iter to simplify merge
perf(dtree): use inner term directly in term->char
perf(ty): faster recognizer functions
perf(dtree): store stack size in iterator
perf: better inlining, remove recursion
perf: monomorphize in Scoped
perf: refactor some hotspots in substitution
refactor: some inline annotations, minor changes
chore(make): modernize `make watch`, ensure consistent flags everywhere
fix: make demo build again
fix: in docker, isntall iter
fix
ok, polymorphism fixed one more time. losing my hair'
chore: update setup for benchs
changed t for u_p
first attempt at fix
polymorphism seems to be working
more
more changes
seemed to have fixed the polymorphism issues. I hate polymorphism
removed ugly hack --- have to find new option
many fixes
max depth can be set up
fix the unification bug;
new heuristics
more
fixes
added better support for projections
improved conservative elim
added documentation for pattern unification
refactored variable renaming
documentation for pragmatic HO unif
some documentation
refactorings
doc: update misleading comments
new heuristics
more
more
removed some code duplication
refactoring -- removed composition
more
more
it seems that eta_qreduce works OK
fix
removed some assertions
various eta reduction bug fixes
fixed bug with quick eta reduction
unit tests
quick reduce fix
merge with changes from 18th
implemented _fast_ eta reduction
bugfixes
integrated pattern with full unif, some bugs
changed pattern unif api
pattern unification compiles
polymorphism fix
added options that solve problems with TPTP I encountered last night
added better dealing with almost FO-problems
fixed
probably fixed ;
semi-fixed
there are still two bugs
funs for pattern unif compile
more
delayed var-var pairs
some unif fixes
fixed unif bug
some fixes
more
renamed files
new heuristics
more
fixed fp indexing bug
removed assertions
chore: disable bytecode versions of binaries for now
refactor: handle hyper-resolution steps
refactor: use the new API of msat.0.8
fix(tests): use Iter there too
switched to fp indexing
made fingerprinting polymorphic
imitation first
chore: migrate to `iter` instead of `sequence`
added conservative elimination rule
option for choosing unif level and supext bug fix
some asserts on
more
disabled imitation after identification
seems to be working
many things work, unit testing setup
added support for imitation and projection of variables
some bugfixes
small bugfix
fixed None addition
Changed every JPUnif to PVUnif
added depth
more
added rigid-rigid
new unif algorithm
new unification
more
loops
more'
code cleanup
new fp
kbo weights
portfolio
selection
removed inject_desctruct for now, needs to be debugged
some fixes
injectivity recognition works
added functions that recognize injectivity axioms
subsumption fix
optimization
fixed variable uniqueness in supEXT
NOW THE BUG IS SQUEEZED!
fixed serious bug with shifting
first version that contains free vars for supEXT
another defunfold fix
ready for starexec
i forgot that there is left and right;
actual fix
semi unfold fix
def unfolding should finally work correctly
stupid def unfold fix
removed some debug print
ho-def-unfold DB shifting fix
some fixes
fixed varleak bug
supEXT skolemization first implementation, untested
added skolem/unskolem support
chore: make build a bit more robust wrt warn-error
solve unification _sub_problems using pattern unification
supext bugfixes
fully added support for supext, testing now
supEXT rules complie
supext indexing
adde some support for supav, need to fix indexing
some fixes
getting reading to do everything with one superposition engine
added variable unleaking support
demod_in_var_args added. demodulates all terms, restrict to atomic type terms in complete mode
added option to only demodulate under lambdas, off by defaut
disabling some parameters
test: add new example about functional equality on |domain|=1
fixed polymorphic constant unfolding
added dealing with lambdas in sup indexing
fixed a small bug in dealing with skolems
fixed the normalization bug
added cmd line option, it seems that temrs are not beta reduced after unfolding the def
fixes
definition making fixes
def unfolding works
first step towards unfoldable definitions
eta-expansion now works on subterms
factored out ho_normalization
merged conjecture relative clause weight
new kbo merged
dealing with lambda-patterns semi-gracefully
unit tests pass
cannot be eta expanded inside the function
add eta reduction
fixed
test for patterns
chore: prepare for sequence→iter deprecation warnings
chore: tighten bounds on msat
doc: add `details` tag to the description of the format
doc: update readme a bit
chore: remove old symlink, add `zipperposition.sh` script
chore: add menhir as a dep
all penalties must be at least 1
take maximum of penalties in binary inferences
multiplicative penalty
multiplicative penalty
supAV cli flag
cli options for penalties
assume eta-expanded terms in JP_unif and Ordering
JP Unif profiling
add skolem in testUnif
testUnif refactoring
bugfix: respect mandatory args in lift_term function of Unif
complete_eq_args for polymorphic types
Eta expand instead of reduce for complete ho mode
bugfix: DB shift in InnerTerm.replace_m + tests
stream penalties for unary inferences
stream penalties setup in sup and supav
/!\ not compiling /!\ minor returned type modification
bugfix: disagreeing subterms under builtins may have different types
bugfix: no imitation of lambda-expressions
add JP unif test
bugfix: pos ext rule may not remove mandatory args
minor changes to JP unif
/!\ not compiling /!\ recovery of clause penalties for the streams in progress
bugfix: handling of subterms in arithmetic literals
Unif: bugfix pattern unification + tests
TPTP parser: (~) for "not" as a constant
eta reduction optimization + tests
mirror changes for simultaneous sup
SupAV passive
supav rulename
SupAV active
JP Unif: minor edit
assert in remove_var_args
JP Unif: minor edit
fix eta reduction
JP Unif: use Unif.FO to unify type args
perf: use profile=release
make floor (and co) a function from real to real
avoid List.hd and List.tl
two bugfixes for polymorphic function types
bugfix JP Unif for polymorphic terms with different number of args
add fair extraction scheme (one element out of all streams) to stream queue 1/6 times
matching bugfix (bug allowed leaking of DB indices) + test
make floor (and co) a function from real to real
avoid List.hd and List.tl

## 1.5.1

- compatibility with ocaml 4.08
- move to sneeuwballen organization
- migrate to dune
- changes in HO
- use msat.0.8
- migrate to `iter` instead of `sequence`
- make build a bit more robust wrt warn-error
- add travis CI

- perf improvements
- perf: use profile=release
- make floor (and co) a function from real to real
- two bugfixes for polymorphic function types
- fix(demo): update resolution demo so it builds again
- fix(superposition): do not try to rewrite type arguments
- chore: update opam files to 2.0
- move to alcotest for tests
- improve ho selection restrictions
- perf: optimize `debugf` function (closes #21)
- support more cases in THF parser
- less selection restrictions for lfhol-calculi
- cli option for fool
- bugfix: KBO variable balance for applied variables
- In TPTP, interpret untyped variables as $i, not as a fresh type variable
- Don't rewrite heads in subsumption module
- complete variables one-by-one in complete_eq_args (ArgCong)
- dont rewrite heads in the presence of type variables or nonmonotonicity
- loosen assertion in whnf_deref_rec
- Use type arguments in orderings
- missing supatvars flag should switch off hidden supatvars

## 1.5

- be compatible with sequence >= 1.0
- cli option to switch off maximal number of variables per clause
- Dockerfile and instructions to build a docker image
- add eta-reduction to `LLTerm`
- update phases API + params so it's easier to use from utop
- move to jbuilder

- fail early when unifying a variable and a polymorphic constant
- More realistic test to expose a bug in unification of polymorphic terms
- upper bound on msat and deps on logtk
- fix for llprover (use congruence correctly for poly equalities)
- printer for congruence
- cache llproof checking result, display it in llproof-printing
- refactor proof checker to look more like a tableaux prover + dot printing
- llprover: hack to allow checking of rewriting steps that occur under binders
- split proof checker into its own module `LLProver`
- add linear expressions and arith predicates in `LLTerm`
- make demodulation more robust
- bugfix in `Type.apply` for polymorphic type arguments
- stop positive extensionality rule from removing type arguments
- moved detection for "distinct object" syntax into TypeInference
- omit type declarations for distinct objects in TPTP output
- bugfix restrict_to_scope: recursive call when variable already in scope
- bugfix: type of polymorphic application in app_encode tool
- bugfix: app_encode extensionality axiom needs type arguments
- `fo_detector` tool to count problems with applied variables
- clean up Subst module
- bugfix: wrong polymorphic types in returned unifier
- remove hornet from makefile, improve logitest targets
- remove hornet
- better type error messages
- make `Subst.apply` tailrec
- "int" mode for variable purification
- bugfix: unquote identifiers in TPTP parser

## 1.4

- remove inlining on parsers
- cli option for ext-neg rule
- add `--check-types` for checking types deeply in new clauses
- Add ExistsConst (??) and ForallConst (!!) to TPTP parser
- TPTP parser: allow function types as THF terms
- add cli option `-bt` (alias to `--backtraces`)
- completion of equalities with λ-abstractions as RHS in type inference
- THF parser: allow for `@` applications in types
- cli flag for ext_pos
- App encode: binary for app-encoding HO applications into FO

- bugfix in ho unification
- in unification, fix order in which bound variables are added to env
- bugfix in unification (would produce wrong type)
- do not simplify in demodulation
- Add StarExec instructions to readme
- bugfixes `app_encode`
- β-normalize rewrite rules that are eq-completed
- uniform output of “SZS status” instead of “SZS Status”
- auto flattening of applications in STerm
- fix `examples/ho/extensionality1.zf` by forbidding some HO demodulations
- fix tag managing (and therefore proof checking) for `Lit.is_absurd`
- bugfix in proof checking related to instantiation
- bugfix in NPDtree
- more elegant and robust sup-at-var condition
- sup-at-var condition with polymorphism
- remove literal comparison by constraint
- no selection of literals containing ho variables
- Stricter sup-at-vars condition
- purify naked variables

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
- don't do sup at vars by default
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
- opam file (for easy development version)

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
    * do not rename variables for skolemized formulas (otherwise variables won't match)
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
- more abstract requirement for Clause index (feature vector...)
- cli flag to disable ordering generation in hysteresis
- hooks for `Cnf`

note: git log --no-merges previous_version..HEAD --pretty=%s
