Getting Started
===============

Requirements
------------

You will need OCaml >= 4.00.1 or higher with ocamlbuild,
`zarith <https://forge.ocamlcore.org/projects/zarith/>`_
and the standard library.

Some modules come from
`ocaml-containers <https://github.com/c-cube/ocaml-containers/>`_ and
are packaged with the library.

Download
--------

You can download archives `on github <https://github.com/c-cube/logtk/releases>`_
or use git directly :

.. code-block:: sh

   git clone https://github.com/c-cube/logtk.git

Compilation
-----------

Additional sub-libraries can be built if their respective dependencies
are met, and the appropriate ``./configure --enable-foobar`` flag was set.
For instance, to build the *meta-prover* library (used to detect axiomatic
theories), you should run

.. code-block:: sh

    $ ./configure --enable-meta

If `menhir <http://cristal.inria.fr/~fpottier/menhir/>`_ is installed, the
parsers library ``Logtk_parsers`` can be built with

.. code-block:: sh

    $ ./configure --enable-parsers

If you have installed `qcheck <https://github.com/c-cube/qcheck/>`_, for instance
via ``opam install qcheck``, you can enable the property-based testing and
random term generators with

.. code-block:: sh

    $ ./configure --enable-qcheck --enable-tests
    $ make tests

Several tools are shipped with ``Logtk``, including a CNF converter, a type-checker,
etc. They are built if the flag ``--enable-tools`` is set. API Documentation
will be built provided ``--enable-docs`` is set.

After the configuration is done, to build the library, documentation and tools
(given the appropriate flags are set), type in a terminal located in the root
directory of the project:

.. code-block:: sh

    $ make

Installation
------------

If you use ``ocamlfind`` (which is strongly recommended),
installation/uninstallation are just:

.. code-block:: sh

    $ make install
    $ make uninstall
