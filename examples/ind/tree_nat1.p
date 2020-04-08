
include('tree.ax').
include('nat.ax').

% size(node(L,_,R)) > size(L)

tff(the, conjecture, ![L:tree, R:tree, X]: lt(size(L), size(node(L,X,R)))).
