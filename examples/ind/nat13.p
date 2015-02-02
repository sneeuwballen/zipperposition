
include('nat.ax').

tff(1, axiom, ![X:nat]: (p(X) => p(s(X)))).
tff(2, axiom, p(z)).

tff(the, conjecture, ![X:nat]: p(X)).
