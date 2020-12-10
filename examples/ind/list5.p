include('list.ax').

tff(the, conjecture, ![X:nat, L:list]:
    count(X, cons(X,L)) = plus(s(z), count(X,L))).

