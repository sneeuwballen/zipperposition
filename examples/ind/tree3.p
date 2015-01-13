
include('tree.ax').

% mem(X, T1) & subtree(T1, T2) => mem(X, T2)

tff(the, conjecture, ![X, T1:tree, T2:tree]:
    ((mem(X, T1) & subtree(T1, T2)) => mem(X, T2))).
