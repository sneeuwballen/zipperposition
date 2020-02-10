List of Modules
===============

The most up-to-date reference to the list of modules is in the ``_oasis`` file
at the root of the project. This file describes how to build parts of the
library.

In this document we give a high-level view of what every module in ``Logtk``
does.

Main Library
------------

The main library contains a single packed module, ``Logtk``, which depends
only on ``zarith`` (a wrapper around `GMP <https://gmplib.org/>`_ used
for arbitrary-precision arithmetic) and ``unix`` (the standard OCaml
wrapper to the Posix APIs). This module
contains other modules, for instance ``Logtk.FOTerm`` for representing
first-order terms. It is often useful to rename modules prior to their
use, for instance

.. code-block:: ocaml

   module T = Logtk.FOTerm
   module P = Logtk.Position

This technique allows to keep identifiers short, without using the ``open``
directive that makes code hard to read (one cannot easily find which opened
module some identifier comes from).

``Logtk``
    * ``Symbol``: representation of logical constants, including text symbols and numeric symbols (using ``Zarith``).
    * ``ScopedTerm``: common internal representation for terms, formulas, etc.  that handles De Bruijn indices, substitutions, and hashconsing.
    * ``PrologTerm``: the dual of ``ScopedTerm``, an untyped AST with locations but no hashconsing nor scoping.
    * ``FOTerm``: first-order typed terms (built on top of ``ScopedTerm``)
    * ``HOTerm``: higher-order typed terms
    * ``Formula``: formulas parametrized by the terms at their leaves.
        - ``Formula.FO``: first-order formulas (with typed terms).
    * ``Type``: polymorphic types with explicit quantification, built on
        top of ``ScopedTerm``, used by ``FOTerm`` and ``HOTerm``.
    * ``Unif``: unification algorithms, both unary and n-ary.
        - ``Unif.FO``: specialization for ``FOTerm``
        - ``Unif.Ty``: specialization for ``Type``
        - similar sub-modules.
    * ``Substs``: substitutions on free variables for types and terms.
    * ``DBEnv``: De Bruijn environments for bound variables.
    * ``Signature``: map from symbols to types
    * ``TypeInference``: Hindley-Milner-like type inference algorithm, that converts ``PrologTerm`` to typed terms and formulas.
    * ``Precedence``: total ordering on symbols.
    * ``Ordering``: orderings on terms, including LPO and KBO (parametrized by ``Precedence``).
    * ``Position``: positions in terms (paths in trees)
    * ``Cnf``: transformation of formulas into *Clause Normal Form*
    * ``Index``: definition of term index signatures. Related modules:
        - ``Dtree``: perfect discrimination tree, for rewriting
        - ``NPDtree``: non-perfect discrimination tree, for rewriting and term indexing
        - ``Fingerprint``: fingerprint term indexing
        - ``FastFingerprint``: attempt (failed for now) to make ``Fingerprint`` faster
        - ``FeatureVector``: feature-vector indexing for subsumption
    * ``Rewriting``: rewriting on terms, ordered rewriting, formula rewriting.
    * ``FormulaShape``: detection of some specific formulas (definitions...).
    * ``Skolem``: skolemization and definitional CNF.
    * ``Lambda``: lambda-calculus (beta reduction) on higher-order terms.
    * ``Transform``: computation of fixpoints over transformations of formulas
    * ``Multiset``: low level multiset of elements, with multiset ordering
    * ``Congruence``: simple congruence closure on terms (decides ground equality)
    * and many helpers modules that can be found in ``src/base/lib/``, including locations, iterators, hashconsing, combinators, etc.

Parsers
-------

This sub-library is optional and will only be built if
`menhir <http://gallium.inria.fr/~fpottier/menhir/>`_ (an
excellent parser generator for OCaml) is installed. Currently it
contains parsers for two file formats. Again the library is
a packed module ``Logtk_parsers`` (e.g. ``Logtk_parsers.Ast_tptpt``).

``Logtk_parsers``
    + ``TPTP``:
        - ``Trace_tstp``: proof traces from TSTP provers.
        - ``CallProver``: call a TSTP prover on a problem.
        - ``Parse_tptp``: TPTP parser
        - ``Lex_tptp``: TPTP lexer
        - ``Ast_tptp``: AST yielded by the parser
        - ``Util_tptp``: higher-level interface to the TPTP parser (the one to use)
    + ``HO`` (an ad-hoc format for higher-order terms with multisets and
        extensible records):
        - ``Parse_ho``: parser for a simple Higher-Order format
        - ``Lex_ho``: lexer for a simple Higher-Order format
        - ``Ast_ho``: AST yielded by ``Parse_ho``


Meta-Prover
-----------

A prover that reasons about features of the problem at hand: higher-level
properties of symbols, algebraic structures, etc. See `there <meta_prover>`_
for more details. The library is packed into ``Logtk_meta``.

``Logtk_meta``
    - ``Encoding``: encoding of formulas and terms into HO terms.
    - ``Reasoner``: forward-chaining reasoner on meta-level facts.
    - ``Plugin``: bridge between HO terms and their proof-level meaning.
    - ``Prover``: general interface to the meta-prover.

Arbitrary Instances
-------------------

This module is only built if `qcheck <https://github.com/c-cube/qcheck/>`_
(a random property-testing library similar to Haskell's QuickCheck) is
installed. It provides random generators for terms, formulas, etc.
The library is packed into ``Logtk_arbitrary``.

- ``Logtk_arbitrary`` (random generators for ``qcheck`` random-testing, optional)
    - ``ArTerm``: generation of random terms
    - ``ArForm``: random first order formulas
    - ``ArType``: random types
    - ``ArSignature``: random signatures
    - ``ArSymbol``: random symbols
    - ``ArPattern``: random meta-patterns

