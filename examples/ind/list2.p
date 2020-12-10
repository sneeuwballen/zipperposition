
include('list.ax').
include('nat.ax').

% very simple double-induction

tff(ty_p, type, p : nat > $o).
tff(ty_q, type, q : list > $o).

tff(p_1, axiom, p(z)).
tff(p_2, axiom, ![N:nat]: (p(N) => p(s(N)))).

tff(q_1, axiom, q(nil)).
tff(q_2, axiom, ![N:nat, L:list]:
    ((p(N) & q(L)) => q(cons(N,L)))).

tff(the, conjecture, ![L:list]: q(L)).
