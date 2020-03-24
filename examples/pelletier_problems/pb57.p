
fof(ax1, axiom, f(g(a,b), g(b,c))).

fof(ax2, axiom, f(g(b,c), g(a,c))).

fof(transitivity, axiom, ! [X,Y,Z] : ((f(X,Y) & f(Y,Z)) => f(X,Z))).

fof(goal, conjecture, f(g(a,b), g(a,c))).
