Meta-Prover Design
==================

Intro
-----

The *meta-prover* is a software component that is responsible for reasoning
about **properties** of logic symbols, but not for solving the actual
problem at hand. It deals with the higher-level structure of a problem, more
at the level of meta-data than the raw axioms given by the user.
See [BurelCruanes2013]_ for more details.

Structure
---------

The meta-prover has two main tasks to carry out:

#. Scanning for **patterns** in clauses it is given (from a running resolution
    or superposition prover, or from an input file...). A successful scan
    may add **facts** about the problem (presence of a given axiom instance).
#. Reasoning over known facts and structures (theories, lemmas, etc.), using
    both a known **knowledge base** (definition of theories, known lemmas)
    and the results of scanning.

Implementation
--------------

The logic representation of the meta-prover is a form of ``CurriedFOTerm``, ie.
first-order terms with currying, no quantifiers. The variants are:

- ``Var : int -> term``
- ``TypeAt : type * term -> term``
- ``At : term * term -> term``
- ``Multiset : term list -> term``
- ``Record : (string*term) list -> term``

Typing should be as usual, except that records always have the type ``record``,
and multisets always have the same type as their arguments. Multisets are
used for representing clauses.

Scanning
^^^^^^^^

We need a specific mix of matching and alpha-equivalence checking for
scanning **concrete clauses** so as to find instances of **patterns**. A
**pattern** is just a term in which all salient function symbols (which occur
in some structure at the meta-level) as *abstracted* into variables. The
meta-prover must remember which variables are regular universal variables,
and which ones are abstracted functions.

The scanning operation must *bind* abstracted variables to concrete terms
(the matching part) but cannot bind universal variables to anything else than
other universal variables (the alpha-equivalence part). Binding type variables
is ok.

For instance, if a pattern for commutativity is ``F X Y = F Y X`` with
abstracted variable ``F``, then scanning the concrete clause ``g a X Y = g a Y X``
succeeds by binding ``F`` to the concrete term ``g a``, and binding
universal variables ``X`` and ``Y`` to other universal variables.

A kind of basic indexing could be done by hashing the structure of the term,
but that doesn't look trivial

Reasoning
^^^^^^^^^

As all rules are Horn clauses, of the shape ``A <- B_1, ..., B_n``, or simple
atoms that we call **facts**. Since we embed universal variables, or type
variables, it actually isn't always true that atoms are ground. So, the idea

Biblio
------

.. [BurelCruanes2013] http://hal.inria.fr/hal-00919759
