
include('list.ax').
include('nat.ax').

% N <= length(L) => take N L @ drop N L = L

tff(the, conjecture, ![N:nat, L:list]:
    (leq(N, length(L)) => append(take(N,L), drop(N,L)) = L)).
