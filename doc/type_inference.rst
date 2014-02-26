Type Inference
==============

We use a kind of Hindley-Milner algorithm to infer types.
Two main constraints are to be respected:

- compatibility with TPTP (THF/TFF1), which forces parametric polymorphism
- flexibility for the meta-prover

We also have interesting extensions such as extensible records.

Sketch of the algorithm
-----------------------

The algorithm takes as input a ``PrologTerm.t``, i.e. an unscoped, untyped
AST with some annotations (``PrologTerm.Column (t,ty)``) and a signature.
The signature is used (and updated) to map constants to their types.

Only declared constants can be polymorphic, because when we meet
a new symbol (or a variable) that is applied to arguments, there is no
**syntactic** way to know which arguments are terms and which are types.
Therefore they are all assumed to be terms unless the signature specifies
the converse explicitely.

For ``HOTerm`` inference, application is left-parenthesed, so when a symbol
requires ``n`` type arguments and ``m`` term arguments, we assume the ``n`` first
arguments to be types.

