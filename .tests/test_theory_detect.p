

% looks like associativity?

tff(a, axiom, ![X]: f(X,f(X,X)) = f(f(X,X),X)). % wrong!

tff(b, axiom, ![Z,Y,X]: g(X,g(Y,Z)) = g(g(X,Y),Z)).

tff(c, axiom, ![Z,Y,X]: h(h(X,Y),Z) = h(X,h(Y,Z))).

tff(d, axiom, ![Y,X]: h(X,Y) = h(Y,X)).
