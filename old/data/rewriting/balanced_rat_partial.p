% symmetric presentation of relative integers (see paper)

% constructors are o (zero), z0(N) = 3*N, z1(N) = 3*N+1, zj(N) = 3*N-1

val o : int.

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

% rational numbers.

% New symbols are 
% - sign(X) = -1, 0 or 1 depending on the sign of x
% - signOrElse(X,Y) = Y if X=0, sign(X) otherwise
% - abs(X) = X if X >= 0, -X otherwise
% - absOrElse(X,Y,Z) = sign(X).Y if X != 0, sign(Z).Y otherwise
% - min(X,Y) = X if abs(X)<=abs(Y), Y otherwise
% - min1(X,Y) = X if abs(3X+1) <= abs(3Y+1), Y otherwise
% - minz(X,Y) = X if abs(3X-1) <= abs(3Y-1), Y otherwise
% - minOrElse(X,Y,Z,U) = Y if (X > 0 or (X=0 and U >= 0)), Z otherwise

sign X --> signOrElse X o.
signOrElse o X --> X.
signOrElse (z0 X) Y --> signOrElse X Y.
signOrElse (z1 X) Y --> signOrElse X (z1 o).
signOrElse (zj X) Y --> signOrElse X (zj o).

abs X --> absOrElse X X o.
absOrElse (z0 X) Y Z --> absOrElse X Y Z.
absOrElse (z1 X) Y Z --> absOrElse X Y (z1 o).
absOrElse (zj X) Y Z --> absOrElse X Y (zj o).
absOrElse o X o --> o.
absOrElse o X (z1 o) --> X.
absOrElse o X (zj o) --> (opp X).

min X Y --> minOrElse (minus (abs X) (abs Y)) X Y o.
min1 X Y --> minOrElse (minus (abs (z1 X)) (abs (z1 Y))) X Y o.
minj X Y --> minOrElse (minus (abs (zj X)) (abs (zj Y))) X Y o.
minOrElse (z0 X) Y Z U --> minOrElse X Y Z U.
minOrElse (z1 X) Y Z U --> minOrElse X Y Z (z1 o).
minOrElse (zj X) Y Z U --> minOrElse X Y Z (zj o).
minOrElse o X Y o --> X.
minOrElse o X Y (z1 o) --> X.
minOrElse o X Y (zj o) --> Y.

% now to represent fractions, a symbol divGCD(X,Y) is introduced with
% the semantics that divGCD(X,Y) = X / gcd(X,Y)
divGCD o X --> o.
divGCD X o --> sign X.
divGCD (z0 X) (z0 Y) --> divGCD X Y.
divGCD (z0 X) (z1 Y) --> z0 (divGCD X (z1 Y)).
divGCD (z0 X) (zj Y) --> z0 (divGCD X (zj Y)).
divGCD (z1 X) (z0 Y) --> divGCD (z1 X) Y.
divGCD (z1 X) (z1 Y) --> plus (z0 (divGCD (minus X (min1 X Y)) (z1 Y)))
                              (divGCD (min (z1 X) (z1 Y)) (minus X Y)).  % won't pass... (nor the following one)
divGCD (z1 X) (zj Y) --> plus (z0 (divGCD (minus X (minj (opp X) Y)) (zj Y)))
                              (divGCD (min (z1 X) (z1 (opp Y))) (plus X Y)).
divGCD (zj X) (z0 Y) --> divGCD (zj X) Y.
%TODO 2 more rules

% vim:syntax=prolog
