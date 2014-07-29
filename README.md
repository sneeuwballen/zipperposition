# Zipperposition

Automated theorem prover for first-order logic with equality and theories.

## Short summary

Zipperposition is intended to be a superposition prover for full first order logic. The accent
is on flexibility, modularity and simplicity rather than performance, to allow
quick experimenting on automated theorem proving. It generates TSTP traces.

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

The license has changed from GPL to BSD2, since no code remains of
what came from matita or Darwin .

See file LICENSE.

## Build

### Via opam

The recommended way to install Zipperposition is through [opam](http://opam.ocaml.org/).
Deducteam has its [own opam repository](https://gforge.inria.fr/projects/opam-deducteam/)
whose adress is https://gforge.inria.fr/git/opam-deducteam/opam-deducteam.git .
You need to have GMP (with headers) installed (it's not handled by opam).  Once
you installed GMP and opam, type:

    $ opam repository add deducteam https://gforge.inria.fr/git/opam-deducteam/opam-deducteam.git

    $ opam install zipperposition

To upgrade to more recent versions:

    $ opam update

    $ opam upgrade

### Manually

If you really need to, you can download a release on the
following [github page for releases](https://github.com/c-cube/zipperposition/releases).

You first need to install `logtk`, `menhir`, `aez`, `containers` and `sequence`.

    $ ./configure

    $ make install


## Use

Typical usage:

    $ zipperposition -help
    $ zipperposition problem_file [options]
    $ zipperposition -arith ARI114=1.p

to run the prover. Help is available with the option *-help*.

For instance,

    $ zipperposition pelletier_problems/pb47.p -ord rpo6 -progress -timeout 30

