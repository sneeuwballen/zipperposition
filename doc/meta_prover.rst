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

#. Scannings
    for **patterns** in clauses it is given (from a running resolution
    or superposition prover, or from an input file...). A successful scan
    may add **facts** about the problem (presence of a given axiom instance).
#. Reasoning
    over known facts and structures (theories, lemmas, etc.), using
    both a known **knowledge base** (definition of theories, known lemmas)
    and the results of scanning.

Implementation
--------------

The logic representation of the meta-prover is a form of ``CurriedFOTerm``, ie.
first-order terms with currying, no quantifiers. The variants are:

- ``Var : int -> term``
    a universal variable, typed, used for abstracted symbols, and for
    Horn clauses
- ``BVar : int -> term``
    a universal variable, typed, used for problem-level quantification. This
    can only by matched with another bound variable (for alpha renaming).
- ``TypeAt : type * term -> term``
    curried application of a term to a type (for polymorphism)
- ``At : term * term -> term``
    curried application of a term to another term
- ``App : term * term list -> term``
    uncurried application of a term to a list of terms.
- ``Multiset : term list -> term``
    multiset of terms, very handy for representing clauses. Unification
    assumes that no variable occurs directly under the multiset, for
    otherwise full AC-unification would be necessary. All terms must have
    the same type ``tau``, in which case the multiset also has type ``tau``.
- ``Record : (string*term) list * term option -> term``
    record value, with a list of ``field : term`` pairs (with pairwise distinct
    fields) and an optional "remainder" part that must be of type ``record``
    and be a variable. The record also has type ``record``. The remainder
    part is used for unification, for instance ::

        {x:X, y:2 | R} == {x:1, y:Y, color: red}

    succeeds and yields ::

        X=1, Y=2, R = {color:red}

    applying the substitution to the first record yields ::

        {x:1, Y:2, color:red}

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
variables, it actually isn't always true that atoms are ground.

The idea is simply to use a term index, and regular unification, to perform
**unit resolution**. Roughly, the rule is ::

    A <- B_1, B_2, ..., B_n        B
    --------------------------------
         sigma(A <- B_2, ..., B_n)

    if sigma(B_1) = sigma(B)

Biblio
------

.. [BurelCruanes2013] http://hal.inria.fr/hal-00919759
