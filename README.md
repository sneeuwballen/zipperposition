# Zipperposition

## Short summary
Zipperposition is intended to be a superposition prover for full first order logic. The accent
is on flexibility, modularity and simplicity rather than performance, to allow
quick experimenting on automated theorem proving. It generates TSTP proofs, but I
don't have a proof checker yet so the proofs may be totally wrong - the issue is that
some clauses can contain formulas (lazy reduction to CNF), which complicates proof
pretty-printing and checking.

Zipperposition is written in the functional and imperative language
[OCaml](http://caml.inria.fr), now using its concurrent variant [JOcaml](http://jocaml.inria.fr).
The name is a bad play on the words "zipper" (a
functional data structure) and "superposition" (the calculus used by the
prover). Superposition-based theorem proving is an active field of research, so
there is a lot of literature about it; for this implementation my main references
are

* the chapter _paramodulation-based theorem proving_ of the _handbook of automated reasoning_,
* the paper _E: a brainiac theorem prover_ that describes the [E](http://eprover.org) prover by S.Schulz,
* the paper _Superposition with equivalence reasoning and delayed clause normal form transformation_ by H.Ganzinger and J.Stuber

**Disclaimer**: Note that the prover is currently a prototype and is
likely not correct nor complete.

## License
Some files come from [matita](http://matita.cs.unibo.it/), the TPTP
parser/lexer are from [Darwin](http://combination.cs.uiowa.edu/Darwin/), some
datastructures are from [Jean-Christophe Filliâtre](http://www.lri.fr/~filliatr/software.en.html/)
(Hashcons, Leftistheap, Ptmap). Since all of them are under GPL, the present software is also
under GPLv2.

A copy of the GPLv2 is attached to the project, in the file LICENSE.

## Build
You will need jocaml >= 3.12 or higher with ocamlbuild and the standard library. Type in a terminal:

    $ make

It should build the project files (using ocamlbuild).


## Use

Typical usage:

    $ ./zipperposition.native problem_file [options]
    $ ./zipperposition.native problem_file -calculus [delayed|superposition]

to run the prover. Help is available with the option *-help*. The prover
accepts CNF and FOF files for both calculi `-calculus superposition` and
`-calculus delayed` (where reduction to CNF is done during the saturation
process). The prover now embeds its own (simple) reduction to CNF algorithm
for the former case.

For instance,

    $ ./zipperposition.native ./pelletier_problems/pb47.p -calculus delayed -ord kbo -progress -timeout 30

