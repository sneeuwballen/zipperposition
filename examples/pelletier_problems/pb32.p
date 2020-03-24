
fof(ax1, axiom, ! [X] : ((f(X) & (g(X) | h(X))) => i(X))).

fof(ax2, axiom, ! [X] : ((i(X) & h(X)) => j(X))).

fof(ax3, axiom, ! [X] : (k(X) => h(X))).

fof(goal, conjecture, ! [X] : ((f(X) & k(X)) => j(X))).
