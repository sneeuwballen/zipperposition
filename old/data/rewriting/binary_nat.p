% from Raphaël's dedukti code

val true : bool.
val o : nat.

val ite : /\ A bool -> A -> A -> A.
ite _ true A B --> A.
ite _ false A B --> B.

% constructors are o, s0 and s1.
% s0(N) is 2*N, and s1(N) is 2*N+1
s0 o --> o.
succ o --> s1 o.
succ (s0 N) --> s1 N.
succ (s1 N) --> s0 (succ N).

% comparisons

lt o (s0 N) --> true.
lt o (s1 N) --> true.
lt N o --> false.

lt (s0 N) (s0 M) --> lt N M.
lt (s0 N) (s1 M) --> leq N M.
lt (s1 N) (s0 M) --> lt N M.
lt (s1 N) (s1 M) --> lt N M.

gt N M --> lt M N.

leq o N --> true.
leq (s0 N) o --> false.
leq (s1 N) o --> false.

leq (s0 N) (s0 M) --> leq N M.
leq (s0 N) (s1 M) --> leq N M.
leq (s1 N) (s0 M) --> lt N M.
leq (s1 N) (s1 M) --> leq N M.

geq N M --> leq M N.

eq o o --> true.
eq o (s0 N) --> false.
eq o (s1 N) --> false.
eq (s0 N) o --> false.
eq (s1 N) o --> false.
eq (s0 N) (s0 M) --> eq N M.
eq (s0 N) (s1 M) --> false.
eq (s1 N) (s0 M) --> false.
eq (s1 N) (s1 M) --> eq N M.

% addition and multiplication

plus o N --> N.
plus N o --> N.
plus (s0 N) (s0 M) --> s0 (plus N M).
plus (s0 N) (s1 M) --> s1 (plus N M).
plus (s1 N) (s0 M) --> s1 (plus N M).
plus (s1 N) (s1 M) --> s0 (succ (plus N M)).

mult o N --> o.
mult N o --> o.
mult (s0 N) (s0 M) --> s0 (s0 (mult N M)).
mult (s0 N) (s1 M) --> s0 (plus M (s0 (mult N M))).
mult (s1 N) (s0 M) --> s0 (plus N (s0 (mult N M))).
mult (s1 N) (s1 M) --> s1 (plus (s0 (mult N M)) (plus N M)).

max A B --> ite _ (leq A B) B A.
min A B --> ite _ (leq A B) A B.

% vim:syntax=prolog
