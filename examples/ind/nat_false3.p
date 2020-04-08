include('nat.ax').

tff(ty_p, type, p : nat > $o).

tff(p1, axiom, ![N:nat]: (p(N) => p(s(s(N))))).
tff(p2, axiom, p(z)).

tff(the, conjecture, ![N:nat]: p(N)).
