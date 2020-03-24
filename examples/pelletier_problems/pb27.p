
fof(ax1, axiom, ? [X] : (f(X) & ~ g(X))).

fof(ax2, axiom, ! [X] : (f(X) => h(X))).

fof(ax3, axiom, ! [X] : ((j(X) & i(X)) => f(X))).

fof(ax4, axiom, ? [X] : (h(X) & ~ g(X)) =>
  ! [X] : (i(X) => ~ h(X))).

fof(goal, conjecture, ! [X] : (j(X) => ~ i(X))).
