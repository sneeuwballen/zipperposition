% x+ s y = s (x+y)

include('nat.ax').

tff(the, conjecture, ![X:nat, Y:nat]: plus(X,s(Y)) = s(plus(X,Y))).

