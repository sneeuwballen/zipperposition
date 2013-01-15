
cnf(1, axiom, ~p(X,Y,Z) | ~p(X,Y,Z2) | Z=Z2).
cnf(2, axiom, ~q(X,Y,Z) | ~q(X,Y,Z2) | Z=Z2).
cnf(3, axiom, p(X,Y,f(X,Y))).
cnf(4, axiom, f(f(X,Y),Z) = f(X,f(Y,Z))).
cnf(5, axiom, f(X,Y) = f(Y,X)).
cnf(goal, negated_conjecture, ~h(X,Y)).

