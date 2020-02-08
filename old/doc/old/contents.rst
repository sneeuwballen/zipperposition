Documentation
=============

Presentation
------------

Logtk is a free library for manipulating formal logic terms and formulas. It
is licensed under the BSD2 license for the `OCaml <http://ocaml.org/>`_ programming
language.

Logtk provides several useful parts for logic-related implementations:

- a library packed in a module ``Logtk``, with terms, formulas, etc.;
- small tools (see directory ``src/tools/``) to illustrate how to use the library
    and provide basic services (type-checking, reduction to CNF, etc.);
- an optional library in a module ``Logtk_meta``,
    to provide reasoning at the problem level, about the presence of axiomatic
    theories. A small file describing a few theories can be found in
    ``data/builtin.theory`` and one of the tools, ``detect_theories``, can be
    used straightforwardly.

Where to go next
----------------

- Compilation/installation instructions `getting started <getting_started>`_
- A `tutorial <tutorial>`_ to learn the basics
- A `more comprehensive list of modules <modules>`_

Contact
-------

If you have troubles of any kind, write to ``simon DOT cruanes AT inria DOT fr``
or to the `bugtracker <https://github.com/c-cube/logtk/issues>`_. Contributions,
patches, and new features (e.g. parsers) are very welcome.
 
Table of Contents
-----------------

.. toctree::

   Getting Started <getting_started>
   Tutorial <tutorial>
   List of modules <modules>
   Hierarchy of term representations <term_hierarchy>
   Meta-Prover <meta_prover>
   Higher-Order indexing <ho_indexing>
   Type Inference <type_inference>
   Hysteresis (preprocessor to E) <hysteresis>
