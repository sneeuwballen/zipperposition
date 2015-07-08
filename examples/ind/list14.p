% X in L ==>  X in L' @ L

include('list.ax').

tff(the, conjecture,
    ![X:nat, L:list, L2:list]:
        (mem(X, L) => mem(X, append(L2, L)))
).


