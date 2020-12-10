% count(X,L) ≤ len(L)

include('nat.ax').
include('list.ax').

tff(the, conjecture, ![X:nat, L:list]:
    leq(count(X,L), length(L))).
