# Zipperposition

## CASC

Note for CASC: to install Zipperposition, you need OCaml >= 4.00.1.
follow those steps:

    $ cd zipperposition-0.2-casc
    $ ./configure

with options `--bindir` for the binaries location, and `--datadir` for the
knowledge files.

    $ make
    $ make install

Make install may require root permissions. Then, you can launch Zipperposition using

    $ zipperposition_launcher $FILE $TIMEOUT

where FILE is the problem file, and TIMEOUT a timeout in seconds. Zipperposition
is standalone, except for the `kb` file that stores some (basic) knowledge
about theories. Solutions, when they are found, (try to) follow
the SZS Ontology and TSTP output format (with FOF refutations). Outputing
solutions into graphs (in the DOT format) is also possible by giving the
prover itself (not the launcher) the option `-dot file.dot`. Then, for
instance, `dot -Txlib file.dot` can be used to display the graph.

The regular `README` content follows, you can ignore it.

## Short summary
Zipperposition is intended to be a superposition prover for full first order logic. The accent
is on flexibility, modularity and simplicity rather than performance, to allow
quick experimenting on automated theorem proving. It generates TSTP traces; I did
not find a free proof checker so I wrote a simple one (available on
[a github repository](https://github.com/c-cube/tstp-proof-checker "proof checker"))
that uses external provers (E and SPASS) to check each deduction step. You still
have to trust the (possibly lazy) CNF reduction steps, sadly.

Zipperposition is written in the functional and imperative language
[OCaml](http://caml.inria.fr). The name is a bad play on the words "zipper" (a
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
Some files come from [matita](http://matita.cs.unibo.it/), the TPTP
parser/lexer are from [Darwin](http://combination.cs.uiowa.edu/Darwin/), some
datastructures are from [Jean-Christophe Filliâtre](http://www.lri.fr/~filliatr/software.en.html/)
(Hashcons, Leftistheap, Ptmap). Since all of them are under GPL, the present software is also
under GPLv2.

A copy of the GPLv2 is attached to the project, in the file LICENSE.

## Regular build

You will need OCaml >= 4.00.1 or higher with ocamlbuild and the standard
library.

Some submodules are used:

    $ git submodule update --init

Then, type in a terminal located in the root directory of the project:

    $ make

It should build the project files (using ocamlbuild).
The executable is `zipperposition.native`. If you want to install the prover
in your global path, type:

    $ make install

or, if you want to change the installation directory (default is `/usr/bin/`), type:

    $ make INSTALLDIR=/foo/bar/ install

## Use

Typical usage:

    $ zipperposition -help
    $ zipperposition problem_file [options]
    $ zipperposition problem_file -calculus [delayed|superposition]

to run the prover. Help is available with the option *-help*. The prover
accepts CNF and FOF files for both calculi `-calculus superposition` and
`-calculus delayed` (where reduction to CNF is done during the saturation
process). The prover now embeds its own (simple) reduction to CNF algorithm
for the former case.

For instance,

    $ zipperposition pelletier_problems/pb47.p -calculus delayed -ord kbo -progress -timeout 30

## Knowledge Base

Zipperposition now uses a `Knowledge Base` that contains information about theories,
lemmas, rewriting systems, etc. By default it tries to access
`$ZIPPERPOSITION_HOME/kb`, and loads
the theory file `builtin.theory` (written in a human-readable syntax). If you
wish to deactivate this feature, use `-no-theories` or `-kb /dev/null`.
