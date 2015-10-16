
fof(ax1, axiom, ? [X] : p(X) <=> ? [X] : q(X)).

fof(ax2, axiom, ! [X,Y] :
  ((p(X) & q(Y)) => (r(X) <=> s(Y)))).

fof(goal, conjecture, ! [X] : (p(X) => r(X)) <=>
  ! [X] : (q(X) => s(X))).
