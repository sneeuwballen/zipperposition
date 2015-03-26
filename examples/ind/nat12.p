
include('nat.ax').

% p(n) & q(n) => q(n+1)
% p(n) & q(n+1) => p(n+1)
% p0 et q0
% prove:  !n: p(n)

tff(ty_p, type, p:nat > $o).
tff(ty_q, type, q:nat > $o).

tff(p_1, axiom, p(z)).
tff(q_1, axiom, q(z)).
tff(succ_1, axiom, ![N:nat]: ((p(N) & q(N)) => q(s(N)))).
tff(succ_2, axiom, ![N:nat]: ((p(N) & q(s(N))) => p(s(N)))).

tff(the, conjecture, ![N:nat]: p(N)).
