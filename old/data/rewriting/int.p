% from Raphaël's dedukti code
% relative integers in base 1

val o : nat.
val ite : /\ A bool -> A -> A -> A.
val true : bool.
val false : bool.

ite _ true A B --> A.
ite _ false A B --> B.

% scavenged from peano.p
nat_plus o X --> X.
nat_plus (s X) Y --> s (nat_plus X Y).
nat_mult o X --> o.
nat_mult (s X) Y --> nat_plus Y (nat_mult X Y).

nat_leq o N --> true.
nat_leq (s N) o --> false.
nat_leq (s N) (s M) --> nat_leq N M.

nat_lt o (s N) --> true.
nat_lt N o --> false.
nat_lt (s N) (s M) --> nat_lt N M.

nat_eq o o --> true.
nat_eq o (s N) --> false.
nat_eq (s N) o --> false.
nat_eq (s N) (s M) --> nat_eq N M.

% make(N,M) means N-M

val make : nat -> nat -> int.

make (s N) (s M) --> make N M.

leq (make N M) (make P Q) --> nat_leq (nat_plus N Q) (nat_plus M P).
lt (make N M) (make P Q) --> nat_lt (nat_plus N Q) (nat_plus M P).
geq N M --> leq M N.
gt N M --> lt M N.
eq (make N M) (make P Q) --> nat_eq (nat_plus N Q) (nat_plus M P).

plus (make N M) (make P Q) --> make (nat_plus N P) (nat_plus M Q).
mult (make N M) (make P Q) -->
  make (nat_plus (nat_mult N P) (nat_mult M Q))
       (nat_plus (nat_mult N Q) (nat_mult M P)).
opp (make N M) --> make M N.

max A B --> ite _ (leq A B) B A.
min A B --> ite _ (leq A B) A B.

abs (make (s N) o) --> make (s N) o.
abs (make o (s N)) --> make (s N) o.
abs (make o o) --> make o o.

% vim:syntax=prolog

