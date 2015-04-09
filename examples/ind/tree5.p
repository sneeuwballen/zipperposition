
include('tree.ax').

% p(l) & q(l) & p(r) & q(r) => q(node(l,_,r))
% p(l) & q(l) & p(r) & q(r) => p(node(l,_,r))
% p(empty) et q(empty)
% prove:  !t: p(t)

tff(ty_p, type, p:tree > $o).
tff(ty_q, type, q:tree > $o).

tff(p_1, axiom, p(empty)).
tff(q_1, axiom, q(empty)).
tff(node_1, axiom, ![L:tree, X, R:tree]:
    ((p(L) & q(L) & p(R) & q(R)) => q(node(L,X,R)))).
tff(node_2, axiom, ![L:tree, X, R:tree]:
    ((p(L) & q(L) & p(R) & q(R)) => p(node(L,X,R)))).

tff(the, conjecture, ![T:tree]: p(T)).
