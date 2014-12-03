
tff(ty_s, type, s:nat > nat).
tff(ty_z, type, z:nat).

tff(1, axiom, ![X:nat]: (p(X) => p(s(s(X))))).
tff(2, axiom, p(z)).

tff(the, conjecture, ![X:nat]: p(X)).
