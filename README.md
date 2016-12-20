# Zipperposition

- Automated theorem prover for first-order logic with equality and theories.
- Logic toolkit (`libzipperposition`), designed primarily
  for first-order automated reasoning. It aims
  at providing basic types and algorithms (terms, unification, orderings,
  indexing, etc.) that can be factored out of several applications.

## Short summary

Zipperposition is intended to be a superposition prover for full first
order logic, plus some extensions (datatypes, recursive functions, arithmetic).
The accent is on flexibility, modularity and simplicity rather than
performance, to allow quick experimenting on automated theorem proving. It
generates TSTP traces or graphviz files for nice graphical display.

Zipperposition supports several input formats:

- TSTP (fof, cnf, tff)
- [TIP](https://tip-org.github.io/)
- its own native input, extension `.zf` (see directory `examples/`)

Zipperposition is written in the functional and imperative language
[OCaml](https://ocaml.org). The name is a bad play on the words "zipper" (a
functional data structure) and "superposition" (the calculus used by the
prover), although the current implementation is written in quite an imperative style.
Superposition-based theorem proving is an active field of research, so
there is a lot of literature about it; for this implementation my main references
are:

* the chapter _paramodulation-based theorem proving_ of the _handbook of automated reasoning_,
* the paper _E: a brainiac theorem prover_ that describes the [E](http://eprover.org) prover by S.Schulz,
* the paper _Superposition with equivalence reasoning and delayed clause normal form transformation_ by H.Ganzinger and J.Stuber

**Disclaimer**: Note that the prover is currently a prototype and is
likely not complete. Please don't use it to drive your personal
nuclear power plant, nor as a trusted tool for critical applications.

## License

This project is licensed under the BSD2 license. See the `LICENSE` file.

## Build

### Via opam

The recommended way to install Zipperposition is through [opam](http://opam.ocaml.org/).
You need to have GMP (with headers) installed (it's not handled by opam).  Once
you installed GMP and opam, type:

    $ opam install zipperposition

To upgrade to more recent versions:

    $ opam update

    $ opam upgrade

If you want to try the development (unstable) version, which has more
dependencies (in particular Oasis for the build), try:

    $ opam pin add zipperposition -k git https://github.com/c-cube/zipperposition.git#dev

### Manually

If you really need to, you can download a release on the
following [github page for releases](https://github.com/c-cube/zipperposition/releases).

Look in the file `opam` to see which dependencies you need to install.
They include `menhir`, `zarith`, `containers`,
`oclock`, [msat](https://github.com/Gbury/mSAT) and `sequence`, but
maybe also other libraries. Consider using opam directly if possible.

    $ ./configure

    $ make install

Additional sub-libraries can be built if their respective dependencies
are met, and the appropriate `./configure --enable-foobar` flag was set.

If [menhir](http://cristal.inria.fr/~fpottier/menhir/) is installed, the
parsers library `Libzipperposition_parsers` can be built with

    $ ./configure --enable-parsers

If you have installed [qcheck](https://github.com/c-cube/qcheck/), for instance
via `opam install qcheck`, you can enable the property-based testing and
random term generators with

    $ ./configure --enable-qcheck --enable-tests
    $ make tests


## Use

Typical usage:

    $ zipperposition --help
    $ zipperposition problem_file [options]
    $ zipperposition --arith ARI114=1.p
    $ zipperposition --dot /tmp/foo.dot examples/ind/nat1.zf

to run the prover. Help is available with the option `--help`.

For instance,

    $ zipperposition pelletier_problems/pb47.p --ord rpo6 --timeout 30

Several tools are shipped with Zipperposition, including a CNF converter, a type-checker,
etc. They are built if the flag `--enable-tools` is set. Documentation
will be built provided `--enable-docs` is set.

After the configuration is done, to build the library, documentation and tools
(given the appropriate flags are set), type in a terminal located in the root
directory of the project:

    $ make

If you use `ocamlfind` (which is strongly recommended),
installation/uninstallation are just:

    $ make install
    $ make uninstall

### Library

Zipperposition's library provides several useful
parts for logic-related implementations:

- a library packed in a module `Libzipperposition`, with terms, formulas, etc.;
- a library packed in a module `Libzipperposition_parsers`, with parsers for input formats;
- small tools (see directory `src/tools/`) to illustrate how to use the library
    and provide basic services (type-checking, reduction to CNF, etc.);

## Documentation

See [this page](http://c-cube.github.io/zipperposition/).

There are some examples of how to use the code in `src/tools/`
and `src/demo/`.

