Hysteresis
==========

Hysteresis is a preprocessor for `E <http://www4.informatik.tu-muenchen.de/~schulz/E/E.html/>`_
that can perform several tasks (described in details in the `section about tasks`_:

- detect theories (using :doc:`the Meta Prover <meta_prover/>`), obtaining
  - instances of algebraic theories (group, etc.)
  - rewrite rules on terms (to be oriented, see further)
  - pre-rewrite rules on formulas. See `the section on pre-rewrite rules`_.
- orient term rewrite rules with a LPO ordering, if such an ordering exists;
  the LPO is imposed on E so that the rewrite system if effectively
  enforced by E
- encode (integer) arithmetic problems into rewrite problems, using a
  logarithmic base. This effectively transforms a TFA problem into a regular
  TFF problem that E can deal with [#e_arith]_.


.. _`section about tasks`:

Tasks
-------

Hysteresis can perform several tasks before yielding control to E:

.. _`the section on pre-rewrite rules`:

Pre-rewrite rules
^^^^^^^^^^^^^^^^^

Pre-rewrite rules are rules that rewrite a literal/proposition into a formula.
The most salient example so far is the *magic lemma*: provided a relation
``R(x,y,z)`` is functional (i.e. ``R(x,y,z) & R(x,y,z') => z=z'``
and ``R(x,y,f(x,y))``) we can apply the rule
``R(x,y,z) → z=f(x,y)``. This transforms ugly relations into nice equations
that E can efficiently use for rewriting, especially given that
many TPTP problems use the relational encoding of equations.

Rewrite rules
^^^^^^^^^^^^^

Equations on terms that are oriented left-to-right so that they decide some
equational theory (group theory for instance). We try to find a LPO
precedence that orient all the equations left-to-right, so that if E
uses this precedence, it will effectively implement the rewrite system
and normalize every term with it.

In particular this should be used for arithmetic, where we represent
numbers with terms (in some binary or ternary base) and arithmetic
operations with rewrite rules on those terms.

To orient rules with a LPO ordering
we use `Aez <http://cubicle.lri.fr/alt-ergo-zero/>`_
and the encoding of [Codish]_.


.. rubric:: Footnotes.

.. [#e_arith] It doesn't mean E will easily solve problems, but that it
   has a chance to do so.

.. [Codish] Solving Partial Order Constraints for LPO Termination, Codish & al.
