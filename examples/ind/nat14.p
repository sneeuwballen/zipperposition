include('nat.ax').

tff(1, axiom, ![X:nat]: (p(X) => p(s(s(X))))).
tff(2, axiom, p(z)).
tff(3, axiom, p(s(z))).

tff(the, conjecture, ![X:nat]: p(X)).

