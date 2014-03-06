val zero : nat.

plus zero X --> X.
plus (succ X) Y --> succ (plus X Y).
mult zero X --> zero.
mult (succ X) Y --> plus Y (mult X Y).
% vim:syntax=prolog
