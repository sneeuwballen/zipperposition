
% X in L@[X]

include('list.ax').

tff(the, conjecture, ![X:nat, L:list]: mem(X, append(L, cons(X,nil)))).
