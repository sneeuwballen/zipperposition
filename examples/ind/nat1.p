
% associativity

include('nat.ax').

tff(1, conjecture, ![X:nat, Y:nat, Z:nat]: plus(X,plus(Y,Z)) = plus(plus(X,Y),Z)).
