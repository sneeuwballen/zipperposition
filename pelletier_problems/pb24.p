
fof(ax1, axiom, ~ ? [X] : (s(X) & q(X))).

fof(ax2, axiom, ! [X] : (p(X) => (q(X) | r(X)))).

fof(ax3, axiom, (~ (? [X] : p(X))) => ? [X] : q(X)).

fof(ax4, axiom, ! [X] : ((q(X) | r(X)) => s(X))).

fof(goal, conjecture, ? [X] : (p(X) & r(X))).
