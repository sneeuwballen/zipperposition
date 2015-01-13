
include('tree.ax').

tff(0, axiom, ![X]: q(X)).
tff(1, axiom, ![X:tree, Y:tree, Z]: ((p(X) & p(Y) & q(Z)) => p(node(X,Z,Y)))).
tff(2, axiom, p(empty)).

tff(the, conjecture, ![X:tree]: p(X)).
