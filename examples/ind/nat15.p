
% even(X+X)

include('nat.ax').

tff(type_odd, type, odd: nat > $o).
tff(type_even, type, even: nat > $o).

tff(odd1, axiom, odd(s(z))).
tff(odd2, axiom, ![N:nat]: (even(N) => odd(s(N)))).
tff(even1, axiom, even(z)).
tff(even2, axiom, ![N:nat]: (odd(N) => even(s(N)))).

tff(the, conjecture, ![N:nat]: even(plus(N,N))).
