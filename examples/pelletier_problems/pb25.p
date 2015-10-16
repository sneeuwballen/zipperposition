
fof(ax1, axiom, ? [X] : p(X)).

fof(ax2, axiom, ! [X] : (f(X) => (~ g(X) & r(X)))).

fof(ax3, axiom, ! [X] : (p(X) => (g(X) & f(X)))).

fof(ax4, axiom, ! [X] : (p(X) => q(X)) |
  ? [X] : (p(X) & r(X))).

fof(goal, conjecture, ? [X] : (q(X) & p(X))).
