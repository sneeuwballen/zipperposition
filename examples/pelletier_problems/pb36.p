
fof(ax1, axiom, ! [X] : ? [Y] : f(X,Y)).

fof(ax2, axiom, ! [X] : ? [Y] : g(X,Y)).

fof(ax3, axiom, ! [X,Y] : ((f(X,Y) | g(X,Y)) =>
  ! [Z] : ((f(Y,Z) | g(Y,Z)) => h(X,Z)))).

fof(goal, conjecture, ! [X] : ? [Y] : h(X,Y)).
