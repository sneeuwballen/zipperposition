
include('nat.ax').

% p(n) | q(n) => q(n+1) | p(n+1)
% p0 | q0
% prove:  !n: p(n) | q(n)

tff(ty_p, type, p:nat > $o).
tff(ty_q, type, q:nat > $o).

tff(p_q_0, axiom, p(z) | q(z)).
tff(p_q_s, axiom, ![N:nat]: ((p(N) | q(N)) => (p(s(N)) | q(s(N))))).

tff(the, conjecture, ![N:nat]: (p(N) | q(N))).
