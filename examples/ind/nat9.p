
include('nat.ax').

% (X+Y)-X = Y

tff(the, conjecture, ![X:nat, Y:nat]: minus(plus(X,Y),X) = Y).
