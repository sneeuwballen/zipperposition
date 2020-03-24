
include('list.ax').

% butlast(L @ [x]) = L

tff(the, conjecture, ![X:nat, L:list]: (butlast(append(L,cons(X,nil))) = L)).

