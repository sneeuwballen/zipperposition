# LogTK

Logic toolkit, designed primarily for first-order automated reasoning. It aims
at providing basic types and algorithms (terms, unification, orderings,
indexing, etc.) that can be factored out of several applications.

## License

This project is licensed under the BSD2 license. See the `LICENSE` file.

## Regular build

You will need OCaml >= 4.00.1 or higher with ocamlbuild and the standard
library.

Some submodules are used:

    $ git submodule update --init

Then, type in a terminal located in the root directory of the project:

    $ make

If you use `ocamlfind` (which you should), installation is just

    $ make install

    
