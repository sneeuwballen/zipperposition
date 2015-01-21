# LogTK

Logic toolkit, designed primarily for first-order automated reasoning. It aims
at providing basic types and algorithms (terms, unification, orderings,
indexing, etc.) that can be factored out of several applications.

## License

This project is licensed under the BSD2 license. See the `LICENSE` file.

## Build

### Via opam

We recommand using [opam](http://opam.ocaml.org). Once you installed it,
type

    $ opam repository add deducteam \
        https://gforge.inria.fr/git/opam-deducteam/opam-deducteam.git
    $ opam install logtk

If you want to try the development (unstable) version, try:

    $ opam pin add logtk -k git https://github.com/c-cube/logtk.git#dev

### From sources

You will need OCaml >= 4.00.1 or higher with ocamlbuild
 , [zarith](https://forge.ocamlcore.org/projects/zarith/)
 , [ocaml-containers](https://github.com/c-cube/ocaml-containers/)
 , [sequence](https://github.com/c-cube/sequence/)
and the standard library.

Additional sub-libraries can be built if their respective dependencies
are met, and the appropriate `./configure --enable-foobar` flag was set.
For instance, to build the *meta-prover* library (used to detect axiomatic
theories), you should run

    $ ./configure --enable-meta

If [menhir](http://cristal.inria.fr/~fpottier/menhir/) is installed, the
parsers library `Logtk_parsers` can be built with

    $ ./configure --enable-parsers

If you have installed [qcheck](https://github.com/c-cube/qcheck/), for instance
via `opam install qcheck`, you can enable the property-based testing and
random term generators with

    $ ./configure --enable-qcheck --enable-tests
    $ make tests

Several tools are shipped with `Logtk`, including a CNF converter, a type-checker,
etc. They are built if the flag `--enable-tools` is set. Documentation
will be built provided `--enable-docs` is set.

After the configuration is done, to build the library, documentation and tools
(given the appropriate flags are set), type in a terminal located in the root
directory of the project:

    $ make

If you use `ocamlfind` (which is strongly recommended),
installation/uninstallation are just:

    $ make install
    $ make uninstall

## Usage

Logtk provides several useful parts for logic-related implementations:

- a library packed in a module `Logtk`, with terms, formulas, etc.;
- small tools (see directory `src/tools/`) to illustrate how to use the library
    and provide basic services (type-checking, reduction to CNF, etc.);
- an optional library in a module `Logtk_meta`,
    to provide reasoning at the problem level, about the presence of axiomatic
    theories. A small file describing a few theories can be found in
    `data/builtin.theory` and one of the tools, `detect_theories`, can be
    used straightforwardly.


## Documentation

See [this page](http://cedeela.fr/~simon/software/logtk/).

There are some examples of how to use the code in `src/tools/`
and `src/demo/`.

## List of modules

- `Logtk`
    - `Symbol`: representation of logical constants, including text symbols
        and numeric symbols (using `Zarith`).
    - `ScopedTerm`: common internal representation for terms, formulas, etc.
        that handles De Bruijn indices, substitutions, and hashconsing.
    - `PrologTerm`: the dual of `ScopedTerm`, an untyped AST with locations
        but no hashconsing nor scoping.
    - `FOTerm`: first-order typed terms (built on top of `ScopedTerm`)
    - `HOTerm`: higher-order typed terms
    - `Formula`: formulas parametrized by the terms at their leaves.
        - `Formula.FO`: first-order formulas (with typed terms).
    - `Type`: polymorphic types with explicit quantification, built on
        top of `ScopedTerm`, used by `FOTerm` and `HOTerm`.
    - `Unif`: unification algorithms, both unary and n-ary.
        - `Unif.FO`: specialization for `FOTerm`
        - similar sub-modules.
    - `Substs`: substitutions on free variables for types and terms.
    - `DBEnv`: De Bruijn environments for bound variables.
    - `Signature`: map from symbols to types
    - `TypeInference`: Hindley-Milner-like type inference algorithm,
        that converts `PrologTerm`s to typed terms and formulas.
    - `Precedence`: total ordering on symbols.
    - `Ordering`: orderings on terms, including LPO and KBO (parametrized
        by `Precedence`).
    - `Position`: positions in terms (paths in trees)
    - `Cnf`: transformation of formulas into *Clause Normal Form*
    - `Index`: definition of term index signatures. Related modules:
        - `Dtree`: perfect discrimination tree, for rewriting
        - `NPDtree`: non-perfect discrimination tree, for rewriting and term indexing
        - `Fingerprint`: fingerprint term indexing
        - `FastFingerprint`: attempt (failed for now) to make `Fingerprint` faster
        - `FeatureVector`: feature-vector indexing for subsumption
    - `Rewriting`: rewriting on terms, ordered rewriting, formula rewriting.
    - `FormulaShape`: detection of some specific formulas (definitions...).
    - `Skolem`: skolemization and definitional CNF.
    - `Lambda`: lambda-calculus (beta reduction) on higher-order terms.
    - `Transform`: computation of fixpoints over transformations of formulas
    - `Multiset`: low level multiset of elements, with multiset ordering
    - `Congruence`: simple congruence closure on terms (decides ground equality)
    - and many helpers modules that can be found in `src/base/lib/`, including
        locations, iterators, hashconsing, combinators, etc.
- `Logtk_parsers` (optional)
    * TPTP:
        - `Trace_tstp`: proof traces from TSTP provers.
        - `CallProver`: call a TSTP prover on a problem.
        - `Parse_tptp`: TPTP parser
        - `Lex_tptp`: TPTP lexer
        - `Ast_tptp`: AST yielded by the parser
        - `Util_tptp`: higher-level interface to the TPTP parser (the one to use)
    * HO:
        - `Parse_ho`: parser for a simple Higher-Order format
        - `Lex_ho`: lexer for a simple Higher-Order format
        - `Ast_ho`: AST yielded by `Parse_ho`
- `Logtk_meta` (Meta-prover, optional):
    - `Encoding`: encoding of formulas and terms into HO terms.
    - `Reasoner`: forward-chaining reasoner on meta-level facts.
    - `Plugin`: bridge between HO terms and their proof-level meaning.
    - `Prover`: general interface to the meta-prover.
- `Logtk_arbitrary` (random generators for `qcheck` random-testing, optional)
    - `ArTerm`: generation of random terms
    - `ArForm`: random first order formulas
    - `ArType`: random types
    - `ArSignature`: random signatures
    - `ArSymbol`: random symbols
    - `ArPattern`: random meta-patterns

## TODO

- FOTerm:
    - constructor HVar for hidden, possibly non hashconsed, vars that are only
        used to generate fresh variables (AC/HO unif) and disappear
        during renaming
    - AC-normalize terms when head symbol is AC, at hashconsing time
- HOTerm:
    - same as FOTerm: constructor Ty, constructor HVar
- AC-RPO

- tool to print a trace as DOT
- handle existential type var
- handle ite/let in TPTP

- consider moving the proof-checking part of `tools/proof_check_tstp.ml`
    to `trace_tstp` (can be used for embedded proof checking)

- more functors (unif, but also CNF? rewriting? ordering? indexes)

- substitution trees
- path indexing? (better for merges)

