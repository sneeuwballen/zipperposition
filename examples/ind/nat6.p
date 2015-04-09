
% Kersani & Peltier

include('nat.ax').

tff(ty_p, type, p : (nat * $i) > $o).

tff(1, axiom, ![N:nat, X:$i]: (p(N,X) => p(s(N), f(X)))).
tff(2, axiom, p(z, a)).

tff(the, conjecture, ![N:nat]: ?[X:$i]: p(N,X)).
