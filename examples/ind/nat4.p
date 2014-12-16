% double(X) = X+X

include('nat.ax').

tff(ty_double, type, double : nat > nat).

tff(double1, axiom, ![X:nat]: double(s(X)) = s(s(double(X)))).
tff(double2, axiom, double(z) = z).

tff(the, conjecture, ![X:nat]: double(X) = plus(X,X)).
