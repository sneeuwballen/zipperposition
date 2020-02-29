% symmetric presentation of relative integers (see paper
% "Rewrite systems for natural, integral, and rational arithmetic")

% constructors are z_0 (zero), z_3(N) = 3*N, z_3p1(N) = 3*N+1, z_3m1(N) = 3*N-1

val z_3 : $int -> $int.

z_3 z_0 --> z_0.

z_plus z_0 X --> X.
z_plus X z_0 --> X.
z_plus (z_3 X) (z_3 Y) --> z_3 (z_plus X Y).
z_plus (z_3 X) (z_3p1 Y) --> z_3p1 (z_plus X Y).
z_plus (z_3 X) (z_3m1 Y) --> z_3m1 (z_plus X Y).
z_plus (z_3p1 X) (z_3m1 Y) --> z_3 (z_plus X Y).
z_plus (z_3m1 X) (z_3p1 Y) --> z_3 (z_plus X Y).
z_plus (z_3p1 X) (z_3p1 Y) --> z_3m1 (z_plus X (z_plus Y (z_3p1 z_0))).
z_plus (z_3m1 X) (z_3m1 Y) --> z_3p1 (z_plus X (z_plus Y (z_3m1 z_0))).

z_opp z_0 --> z_0.
z_opp (z_3 X) --> z_3 (z_opp X).
z_opp (z_3p1 X) --> z_3m1 (z_opp X).
z_opp (z_3m1 X) --> z_3p1 (z_opp X).

z_minus X Y --> z_plus X (z_opp Y).

z_mult X z_0 --> z_0.
z_mult X (z_3 Y) --> z_3 (z_mult X Y).
z_mult X (z_3p1 Y) --> z_plus X (z_3 (z_mult X Y)).
z_mult X (z_3m1 Y) --> z_plus (z_3 (z_mult X Y)) (z_opp X).

% z_positive or null
val z_positive : $int -> z_bool.

z_positive X --> z_positiveAux X b_true.
z_positiveAux z_0 Y --> Y.
z_positiveAux (z_3 X) Y --> z_positiveAux X Y.
z_positiveAux (z_3p1 X) Y --> z_positiveAux X b_true.
z_positiveAux (z_3m1 X) Y --> z_positiveAux X b_false.

z_geq X Y --> z_positive (z_minus X Y).
z_gt X Y --> z_positive (z_plus (z_minus X Y) (z_3m1 z_0)).
z_leq X Y --> z_geq Y X.
z_lt X Y --> z_gt Y X.

% TODO re-use balanced_rat_partial to implement quotient

% OCaml program to build such terms:
%
% type b_int = Zero | Z1 of b_int | Z0 of b_int | Zj of b_int ;;
% let rec bint_of_int = function
% | 0 -> Zero
% | n when n < 0 -> z_opp (bint_of_int (-n))
% | n when n mod 3 == 0 -> Z0 (bint_of_int (n/3))
% | n when n mod 3 == 1 -> Z1 (bint_of_int (n/3))
% | n -> Zj (bint_of_int ((n+3)/3))
% and z_opp = function
% | Zero -> Zero
% | Zj x -> Z1 (z_opp x)
% | Z0 x -> Z0 (z_opp x)
% | Z1 x -> Zj (z_opp x)
% and int_of_bint = function
% | Zero -> 0
% | Z0 x -> 3 * (int_of_bint x)
% | Z1 x -> 3 * (int_of_bint x) + 1
% | Zj x -> 3 * (int_of_bint x) - 1;;

% vim:syntax=prolog
