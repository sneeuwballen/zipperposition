
include('tree.ax').

% subtree is transitive

tff(the, conjecture, ![T1:tree, T2:tree, T3:tree]:
    ((subtree(T1,T2) & subtree(T2,T3)) => subtree(T1, T3))).
