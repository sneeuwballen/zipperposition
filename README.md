# LogTK

Logic toolkit, designed primarily for first-order automated reasoning. It aims
at providing basic types and algorithms (terms, unification, orderings,
indexing, etc.) that can be factored out of several applications.

## License

This project is licensed under the BSD2 license. See the `LICENSE` file.

## Regular build

You will need OCaml >= 4.00.1 or higher with ocamlbuild,
[menhir](http://cristal.inria.fr/~fpottier/menhir/) and the standard
library. Some modules come from
[containers](https://github.com/c-cube/ocaml-containers/) and are packaged with
the library.

An additional library, `logtk_meta`, can be built if you have
[datalog](https://github.com/c-cube/datalog) installed. For instance:

    $ opam install datalog
    $ ./configure --enable-meta

If you have installed [qcheck](https://github.com/c-cube/qcheck/), for instance
via `opam install qcheck`, you can enable the property-based testing with

    $ ./configure --enable-qcheck
    $ make tests

After the configuration is done, to build the library, documentation and tools,
type in a terminal located in the root directory of the project:

    $ make

If you use `ocamlfind` (which you should), installation/uninstallation are just:

    $ make install
    $ make uninstall

## Usage

Logtk provides several useful parts for logic-related implementations:

- a library packed in a module `Logtk`, with terms, formulas, etc.;
- small tools (see directory `tools/`) to illustrate how to use the library
    and provide basic services (type-checking, reduction to CNF, etc.);
- an optional library in a module `Logtk_meta`, that depends on `Datalog`
    to provide reasoning at the problem level, about the presence of axiomatic
    theories. A small file describing a few theories can be found in
    `data/builtin.theory`.

## List of modules

- Logic related modules:
    - `Symbol`: representation of logical constants, including text symbols
        and numeric symbols
    - `FOTerm`: first-order typed terms
    - `HOTerm`: higher-order typed terms
    - `FOFormula`: first-order formulas (with typed terms)
    - `Basic`: simple terms and formulas with locations and optional typing. Typically used after parsing. Contains:
        - `Basic.Ty`: polymorphic types with quantifiers
        - `Basic.FO`: first order terms
        - `Basic.Form`: first order formulas
        - `Basic.HO`: higher order terms
    - `FOUnif`: unification algorithms on `FOTerm`
    - `HOUnif`: unification algorithms on `HOTerm`
    - `Substs`: variable substitutions for types and terms
    - `Signature`: map from symbols to types
    - `Type`: polymorphic types (à la ML). Operations on types include:
        - `TypeUnif`: unification on types
        - `TypeInference`: Hindley-Milner-like type inference algorithm. Converts untyped terms and formulas to typed terms and formulas
        - `TypeErasure`: conversion from typed terms and formulas to basic ones
        - `TypeConversion`: converts between `Basic.Ty.t` and `Type.t`
    - `Precedence`: total order on symbols
    - `Ordering`: orderings on terms
    - `Position`: positions in terms (paths in trees)
    - `Cnf`: transformation of formulas into Clause Normal Form
    - `Index`: definition of term index signatures. Related modules:
        - `Dtree`: perfect discrimination tree, for rewriting
        - `NPDtree`: non-perfect discrimination tree, for rewriting and term indexing
        - `Fingerprint`: fingerprint term indexing
        - `FastFingerprint`: attempt (failed for now) to make `Fingerprint` faster
        - `FeatureVector`: feature-vector indexing for subsumption
    - `Rewriting`: rewriting on terms, ordered rewriting, formula rewriting
    - `FormulaShape`: detection of some specific formulas (definitions...)
    - `Skolem`: skolemization
    - `Lambda`: lambda-calculus (beta reduction) on higher-order terms
    - `HO`: higher-order operations, including beta-reduction
    - `Transform`: computation of fixpoints over transformations of formulas
    - `Multiset`: low level multiset of elements, with multiset ordering
    - `Trace_tstp`: proof traces from TSTP provers
    - `CallProver`: call a TSTP prover on a problem
    - `Congruence`: simple congruence closure on terms (decides ground equality)
    - `Evaluator`: helps evaluating terms using interpreted symbols
    - `DBEnv`: environment for substituting De Bruijn indices

- Helpers:
    - `Hash`: utils for hashing values
    - `Hashcons`: perfect sharing of structurally equal values (terms...)
    - `Util`: many utils on lists, printing, strings...
    - `Monad`: monadic utils (error monad, list, option)
    - `PartialOrder`: matrix representation of partial orderings, with completion
    - `Options`: global CLI options to be used with `Arg` (set debug level, etc.)
    - `Location`: location within a file
    - `lib/MultiMap`: functional multimap
    - `lib/PersistentHashtbl`: persistent (immutable) hashtable
    - `lib/Sequence`: library of iterators
    - `lib/Bencode`: B-encode serialization format
    - `lib/Bij`: GADT-based serialization/deserialization library
    - `lib/BV`: bitvectors

- TPTP:
    - `Lex_tptp`: TPTP syntax lexer
    - `Parse_tptp`: TPTP syntax parser
    - `Ast_tptp`: TPTP Abstract Syntax Tree
    - `Util_tptp`: high-level API to deal with TPTP problems

- Meta-prover (optional):
    - `MetaPattern`: patterns, that stand for axioms in any signature
    - `MetaReasoner`: type-safe encapsulation of a Datalog engine
    - `MetaKB`: definition of lemmas, theories and axioms
    - `MetaProver`: global state and main interface for the meta-prover
    - `Ast_theory`: AST for theory description files
    - `Lex_theory`: lexer for theory description files
    - `Parse_theory`: parser for theory description files
    - `Signal`: lightweight publish-subscribe pattern

- Arbitrary instances (optional):
    - `ArTerm`: generation of random terms
    - `ArForm`: random first order formulas
    - `ArType`: random types
    - `ArSignature`: random signatures
    - `ArSymbol`: random symbols
    - `ArPattern`: random meta-patterns

## Documentation

See [this page](http://cedeela.fr/~simon/software/logtk/).

## TODO

- FOTerm:
    - constructor Ty (Type.t -> Term.t) for easier type arguments
    - allow partial application (see previous)
    - constructor HVar for hidden, possibly non hashconsed, vars that are only
        used to generate fresh variables (AC/HO unif) and disappear
        during renaming
    - rename mk_node --> app
    - AC-normalize terms when head symbol is AC, at hashconsing time
- HOTerm:
    - same as FOTerm: constructor Ty, constructor HVar
    - careful with scoping of (Ty ty), De Bruijn should live in same space
- basic:
    - type to represent FO/HO terms and also types (factors many things)
- symbol: flags used for hashconsing
- AC-RPO

- tool to print a trace as DOT
- handle existential type var
- handle ite/let in TPTP

- consider moving the proof-checking part of tools/proof_check_tstp.ml
    to trace_tstp (can be used for embedded proof checking)

- functor for unification (parametrized by term/subst and is_var/deref_var/can_unify
    for specific optims like is_ground)
- more functors (unif, but also CNF? rewriting? ordering? indexes)

- typing in meta-prover, with typing preconditions (for expressing sets with types)
    - OR: use untyped HO terms (or maybe hypergraph)
    - OR: (untyped) hypergraph-rewriting based meta-prover

- substitution trees
- path indexing? (better for merges)

- tool to maintain/update a KB
- conditional compilation for parsers on menhir
- think about extensibility of parser for meta-prover

