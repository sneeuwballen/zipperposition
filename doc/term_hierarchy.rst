Term representations
====================

Rationale
---------

The new version of Logtk attemps to reduce code duplication by using a single
core representation for scoped terms, which is ``ScopedTerm``. A second representation,
``PrologTerm``, is used for simple AST representation and manipulation, and
is a bit the dual of ``ScopedTerm``.

``ScopedTerm``'s goal is to centralize all hashconsing, comparison, De Bruijn
indices manipulations, etc. in a single representation. As a consequence,
substitutions (``Substs``) and unification (``Unif``) are defined only once,
and work on scoped terms.

Specialized Representations
---------------------------

``ScopedTerm`` is nice, but it hardly represents any usual structure, because
it is too general. To enforce structural invariants, check types, and provide
a more specific view of an algebraic structure, we use a nice feature of
OCaml called **private aliases**.

For instance, ``Type`` is a representation of polymorphic simple types.
The type ``Type.t`` is declared as ::

    type t = private ScopedTerm.t

which means that a ``Type.t`` is a subtype of ``ScopedTerm.t``. Then, functions
like ``Type.view : Type.t -> Type.view`` (a specific variant) or
``Type.of_term : ScopedTerm.t -> Type.t option`` can be used. Special
constructors like ``var : int -> Type.t`` are also provided and directly
provide instances of ``Type.t`` that have some structural constraints enforced.

Every specialized representation has its own ``kind``, a tag that allows to
check in constant-time whether a ``ScopedTerm.t`` actually is a ``Type.t`` or not.

Hierarchy
---------

- ``ScopedTerm``
  hashconsed terms with scoping and De Bruijn indices for bound variables.
  Almost every term has a "type", that is itself a term (theoretically
  it should be possible to use this representation for calculus of construction)

  - ``FOTerm``
    first-order terms, as usual. No binder, no magic.

  - ``HOTerm``
    higher-order terms with lambdas, multisets and records.

  - ``Type``
    polymorphic types.

  - ``Formula``
    formulas parametrized on the terms that represent atoms. ``Formula.FO``
    is an instance that has ``FOTerm.t`` as terms

- ``PrologTerm``
  terms with named variables, no hashconsing, and not much typing.
