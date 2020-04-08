
% rev o rev = id

%include('tree.ax').

tff(tree_ind, type, tree : $tType, inductive(node, empty)).
tff(ty_empty, type, empty:tree).
tff(ty_node, type, node:(tree * $i * tree)> tree).
tff(ty_rev, type, rev:tree > tree).

tff(rev1, axiom, rev(empty) = empty).
tff(rev2, axiom, ![L:tree, R:tree, X:$i]:
    rev(node(L,X,R)) = node(rev(R),X,rev(L))).

tff(the, conjecture, ![T:tree]: rev(rev(T)) = T).
