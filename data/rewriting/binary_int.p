% from Raphaël's dedukti code, extension to ints

val true : bool.
val false : bool.
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

lt_nat o (s0 N) --> true.
lt_nat o (s1 N) --> true.
lt_nat N o --> false.

lt_nat (s0 N) (s0 M) --> lt_nat N M.
lt_nat (s0 N) (s1 M) --> leq_nat N M.
lt_nat (s1 N) (s0 M) --> lt_nat N M.
lt_nat (s1 N) (s1 M) --> lt_nat N M.

gt_nat N M --> lt_nat M N.

leq_nat o N --> true.
leq_nat (s0 N) o --> false.
leq_nat (s1 N) o --> false.

leq_nat (s0 N) (s0 M) --> leq_nat N M.
leq_nat (s0 N) (s1 M) --> leq_nat N M.
leq_nat (s1 N) (s0 M) --> lt_nat N M.
leq_nat (s1 N) (s1 M) --> leq_nat N M.

geq_nat N M --> leq_nat M N.

eq_nat o o --> true.
eq_nat o (s0 N) --> false.
eq_nat o (s1 N) --> false.
eq_nat (s0 N) o --> false.
eq_nat (s1 N) o --> false.
eq_nat (s0 N) (s0 M) --> eq_nat N M.
eq_nat (s0 N) (s1 M) --> false.
eq_nat (s1 N) (s0 M) --> false.
eq_nat (s1 N) (s1 M) --> eq_nat N M.

% addition and multiplication

plus_nat o N --> N.
plus_nat N o --> N.
plus_nat (s0 N) (s0 M) --> s0 (plus_nat N M).
plus_nat (s0 N) (s1 M) --> s1 (plus_nat N M).
plus_nat (s1 N) (s0 M) --> s1 (plus_nat N M).
plus_nat (s1 N) (s1 M) --> s0 (succ (plus_nat N M)).

mult_nat o N --> o.
mult_nat N o --> o.
mult_nat (s0 N) (s0 M) --> s0 (s0 (mult_nat N M)).
mult_nat (s0 N) (s1 M) --> s0 (plus_nat M (s0 (mult_nat N M))).
mult_nat (s1 N) (s0 M) --> s0 (plus_nat N (s0 (mult_nat N M))).
mult_nat (s1 N) (s1 M) --> s1 (plus_nat (s0 (mult_nat N M)) (plus_nat N M)).

max_nat A B --> ite _ (leq_nat A B) B A.
min_nat A B --> ite _ (leq_nat A B) A B.

% now for relative integers, use pairs (M,N) with meaning (M,N) == M-N

val mkint : nat -> nat -> int.

% should just use successor, but would not terminate
% FIXME check

mkint (s0 M) (s0 N) --> mkint M N.
mkint (s1 M) (s1 N) --> mkint M N.
mkint (s0 (s0 M)) (s1 (s0 N)) --> mkint M (s0 N).
mkint (s1 (s0 M)) (s0 N) --> mkint (s1 M) N.
mkint (s1 (s1 M)) (s0 N) --> mkint (s0 (s1 M)) N.
mkint (s0 M) (s1 (s0 N)) --> mkint M (s1 N).
mkint (s0 M) (s1 (s1 N)) --> mkint M (s0 (s1 N)).

opp (mkint M N) --> mkint N M.

plus (mkint M1 M2) (mkint N1 N2) --> mkint (plus_nat M1 N1) (plus_nat M2 N2).
minus (mkint M1 M2) (mkint N1 N2) --> mkint (plus_nat M1 N2) (plus_nat M2 N1).

mult (mkint M1 M2) (mkint N1 N2) -->
  mkint
     (plus_nat (mult_nat M1 N1) (mult_nat M2 N2))
     (plus_nat (mult_nat M1 N2) (mult_nat M2 N1)).

lt (mkint M1 N1) (mkint M2 N2) --> lt_nat (plus_nat M1 N2) (plus_nat M2 N1).
leq (mkint M1 N1) (mkint M2 N2) --> leq_nat (plus_nat M1 N2) (plus_nat M2 N1).
gt A B --> lt B A.
geq A B --> leq B A.

eq (mkint M1 N1) (mkint M1 N1) --> true.

positive (mkint (s0 M) o) --> true.
positive (mkint (s1 M) o) --> true.
positive (mkint o o) --> true.
positive (mkint o (s0 M)) --> false.
positive (mkint o (s1 M)) --> false.

% sign(X) is 1 if X >= 0, -1 otherwise
sign (mkint (s0 M) o) --> mkint (s0 o) o.
sign (mkint (s1 M) o) --> mkint (s0 o) o.
sign (mkint o o) --> mkint (s0 o) o.
sign (mkint o (s0 M)) --> mkint o (s0 o).
sign (mkint o (s1 M)) --> mkint o (s0 o).

% relative to natural, by absolute value
abs_to_nat (mkint (s0 M) o) --> s0 M.
abs_to_nat (mkint (s1 M) o) --> s1 M.
abs_to_nat (mkint o o) --> o.
abs_to_nat (mkint o (s0 M)) --> s0 M.
abs_to_nat (mkint o (s1 M)) --> s1 M.

% TODO quotient(mkint(M1,N1), mkint(M2,N2)) -->

% vim:syntax=prolog

