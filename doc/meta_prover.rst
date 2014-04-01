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

The logic representation of the meta-prover is a form of ``HOTerm``, ie.
first-order terms with currying, no quantifiers. The relevant variants (a
``Lambda`` variant is omitted) are:

- ``Var : int -> term``
    a universal variable, typed, used for abstracted symbols, and for
    Horn clauses
- ``RigidVar : int -> term``
    a universal variable, typed, used for problem-level quantification. This
    can only by matched with another **rigid variable** (for alpha renaming).
- ``TyAt : type * term -> term``
    curried application of a term to a type (for polymorphism)
- ``At : term * term -> term``
    curried application of a term to another term
- ``Multiset : term list -> term``
    multiset of terms, very handy for representing clauses. Unification of
    multiset is not AC-unification, but instead unifies subterms pairwise.
    All terms must have the same type ``tau``, in which case the multiset
    has type ``multiset(tau)``. This is used both for equational literals,
    with ``eq_lit {| a, b |}`` representing ``a = b`` (resp. ``neq_lit``)
    and clauses (multisets of literals).
- ``Record : (string*term) list * term option -> term``
    record value, with a list of ``field : term`` pairs (with pairwise distinct
    fields) and an optional "remainder" part that must be a variable
    of type ``record(T)`` (with ``T`` some type). The type of the record
    is given by `the typing rules for records`_.
    The remainder part is used for unification, for instance ::

        {x:X, y:2 | R} == {x:1, y:Y, color: red}

    succeeds and yields ::

        X=1, Y=2, R = {color:red}

    applying the substitution to the first record yields ::

        {x:1, Y:2, color:red}bound

.. _the typing rules for records :

The typing rules for records are the following ::

    t_1:tau_1 ....                        t_n : tau_n
    -------------------------------------------------
    {f_1:t_1, ..., f_n:t_n} :             {f_i:tau_i}

    t_i : tau_i                             y : tau_y
    -------------------------------------------------
    {f_1:t_1, ..., f_n:t_n | y} : {f_i:tau_i | tau_y}

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

Scanning is actually done by encoding the problem clauses, in a shallow way,
using ``Multiset`` constructors and ``RigidVar``. For a clause ``c``
let ``c'`` be its encoding; we then add ``holds c' <- .`` to the clauses
of the reasoner so that it can match axiom definitions. Axioms are
described by ``axiom foo <- holds c.`` where ``c`` is the clause that
describes the axiom.

The encoding of clauses is done by:

#. currying applications
#. replacing free variables by ``RigidVar`` instances (in order to force
    unification to be only alpha-equivalence on those)
#. replacing equational literals ``a = b`` by ``eq_lit [a, b]``
    and ``a != b`` by ``neq_lit [a, b]``
#. replacing the clause by ``[l1, l2, ..., ln]`` where the ``li`` are
    the encoded literals.
#. adding ``holds c.`` to the meta-prover where ``c`` is the encoded clause.

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

Indexing
^^^^^^^^

It is important to have some form of indexing. See the page
about :ref:`ho_indexing`.

Biblio
------

.. [BurelCruanes2013] http://hal.inria.fr/hal-00919759
