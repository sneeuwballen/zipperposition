
include('nat.ax').

% X - (X+Y) = 0

tff(the, conjecture, ![X:nat, Y:nat]: minus(X,plus(X,Y)) = z).
