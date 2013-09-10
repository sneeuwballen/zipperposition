# LogTK

Logic toolkit, designed primarily for first-order automated reasoning. It aims
at providing basic types and algorithms (terms, unification, orderings,
indexing, etc.) that can be factored out of several applications.

## License

This project is licensed under the BSD2 license. See the `LICENSE` file.

## Regular build

You will need OCaml >= 4.00.1 or higher with ocamlbuild,
[menhir](http://cristal.inria.fr/~fpottier/menhir/) and the standard
library. Some modules come from
[containers](https://github.com/c-cube/ocaml-containers/) and are packaged with
the library.

An additional library, `logtk_meta`, can be built if you have
[datalog](https://github.com/c-cube/datalog) installed. For instance:

    $ opam install datalog
    $ ./configure --enable-meta


To build the library, documentation and tools, type in a terminal located in
the root directory of the project:

    $ make

If you use `ocamlfind` (which you should), installation is just:

    $ make install

## Usage

Logtk provides several useful parts for logic-related implementations:

- a library packed in a module `Logtk`, with terms, formulas, etc.;
- small tools (see directory `tools/`) to illustrate how to use the library
    and provide basic services (type-checking, reduction to CNF, etc.);
- an optional library in a module `Logtk_meta`, that depends on `Datalog`
    to provide reasoning at the problem level, about the presence of axiomatic
    theories. A small file describing a few theories can be found in
    `data/builtin.theory`.

## Documentation

See [this page](http://cedeela.fr/~simon/software/logtk/).

    
