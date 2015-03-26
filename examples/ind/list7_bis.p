
include('list.ax').

% rev rev = id

tff(ty_rev2, type, rev2 : list > list).
tff(ty_rev_append, type, rev_append : (list * list) > list).

tff(rev2, axiom, ![L:list]: rev2(L) = rev_append(L, nil)).

tff(rev_append_1, axiom, ![X:nat, L1:list, L2:list]:
    rev_append(cons(X,L1), L2) = rev_append(L1, cons(X,L2))).
tff(rev_append_2, axiom, ![L:list]: rev_append(nil, L) = L).

tff(the, conjecture, ![L:list]: rev2(rev2(L)) = L).
