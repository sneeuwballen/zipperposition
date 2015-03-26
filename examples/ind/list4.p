
include('list.ax').
include('nat.ax').

% count X L1 + count X L2 = count X (L1 @ L2)

tff(the, conjecture, ![X:nat, L1:list, L2:list]:
    plus(count(X,L1), count(X,L2)) = count(X,append(L1,L2))).
