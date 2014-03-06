% symmetric presentation of relative integers (see paper)

% constructors are o (zero), z0(N) = 3*N, z1(N) = 3*N+1, zj(N) = 3*N-1

val z0 : nat -> nat.

z0 o --> o.

plus o X --> X.
plus X o --> X.
plus (z0 X) (z0 Y) --> z0 (plus X Y).
plus (z0 X) (z1 Y) --> z1 (plus X Y).
plus (z0 X) (zj Y) --> zj (plus X Y).
plus (z1 X) (zj Y) --> z0 (plus X Y).
plus (z1 X) (z1 Y) --> zj (plus X (plus Y (z1 o))).
plus (zj X) (zj Y) --> z1 (plus X (plus Y (zj o))).

opp o --> o.
opp (z0 X) --> z0 (opp X).
opp (z1 X) --> zj (opp X).
opp (zj X) --> z1 (opp X).

minus X Y --> plus X (opp Y).

mult X o --> o.
mult X (z0 Y) --> z0 (mult X Y).
mult X (z1 Y) --> plus X (z0 (mult X Y)).
mult X (zj Y) --> plus (z0 (mult X Y)) (opp X).

% positive or null
pos X --> posAux X true.
posAux o Y --> Y.
posAux (z0 X) Y --> posAux X Y.
posAux (z1 X) Y --> posAux X true.
posAux (zj X) Y --> posAux X false.

geq X Y --> pos (minus X Y).
gt X Y --> pos (plus (minus X Y) (zj o)).
leq X Y --> geq Y X.
lt X Y --> gt Y X.

% TODO re-use balanced_rat_partial to implement quotient

% OCaml program to build such terms:
%
% type b_int = Zero | Z1 of b_int | Z0 of b_int | Zj of b_int ;;
% let rec bint_of_int = function
% | 0 -> Zero
% | n when n < 0 -> opp (bint_of_int (-n))
% | n when n mod 3 == 0 -> Z0 (bint_of_int (n/3))
% | n when n mod 3 == 1 -> Z1 (bint_of_int (n/3))
% | n -> Zj (bint_of_int ((n+3)/3))
% and opp = function
% | Zero -> Zero
% | Zj x -> Z1 (opp x)
% | Z0 x -> Z0 (opp x)
% | Z1 x -> Zj (opp x)
% and int_of_bint = function
% | Zero -> 0
% | Z0 x -> 3 * (int_of_bint x)
% | Z1 x -> 3 * (int_of_bint x) + 1
% | Zj x -> 3 * (int_of_bint x) - 1;;

% vim:syntax=prolog
