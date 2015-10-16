
fof(ax1, axiom, ! [X] : ((f(X) | g(X)) => ~ h(X))).

fof(ax2, axiom, ! [X] : ((g(X) => ~ i(X)) =>
  (f(X) & h(X)))).

fof(goal, conjecture, ! [X] : i(X)).
