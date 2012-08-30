# Zipperposition

## Short summary
Zipperposition is intended to be a superposition prover for full first order logic. The accent
is on flexibility, modularity and simplicity rather than performance, to allow
quick experimenting on automated theorem proving. It generates TSTP proofs, but I
don't have a proof checker yet so the proofs may be totally wrong.

Zipperposition is written in the functional language [OCaml](http://caml.inria.fr). The name is
a bad play on the words "zipper" (a functional data structure) and "superposition" (the calculus
used by the prover). Superposition-based theorem proving is an active field of research, so
there is a lot of literature about it; for this implementation my references are

* the chapter _paramodulation-based theorem proving_ of the _handbook of automated reasoning_,
* the paper _E: a brainiac theorem prover_ that describes the [E](http://eprover.org) prover by S.Schulz,
* the paper _Superposition with equivalence reasoning and delayed clause normal form transformation_ by H.Ganzinger and J.Stuber

*Disclaimer*: Note that the prover is currently an early prototype and is
likely not correct nor complete.

## License
Some files come from [matita](http://matita.cs.unibo.it/), the TPTP
parser/lexer are from [Darwin](http://combination.cs.uiowa.edu/Darwin/), some
datastructures are from [Jean-Christophe Filliâtre](http://www.lri.fr/~filliatr/software.en.html/)
(Hashcons, Leftistheap, Ptmap). Since all of them are under GPL, the present software is also
under GPLv2.

A copy of the GPLv2 is attached to the project, in the file LICENSE.

## Build
You will need ocaml (3.12 or higher works; 3.11 or lower may work, I did not test). Type

    make

It should build files (using ocamlbuild).


## Use

Typical usage:

    $ ./prover3.sh problem_file [options]
    $ ./prover.sh problem_file [options]
    $ ./src/main.native problem_file -calculus [delayed|superposition]

to run the prover. Help is available with the option *-help*. The scripts `prover3.sh`
and `prover.sh` call the external prover [E](http://eprover.org) for reduction to CNF;
the former is for [TPTP3](http://tptp.org) syntax, and the latter for old TPTP syntax.
You can also call the binary directly (it parses TPTP3 syntax); it accepts CNF files
with `-calculus superposition`, and CNF or FOF files for `-calculus delayed` (where
reduction to CNF is done during the saturation process).

For instance,

    ./prover3.sh pelletier_problems/pb23.p -timeout 5

