
fof(ax1, axiom, ! [X,Y] : (q(X,Y) <=>
  ! [Z] : (f(Z,X) <=> f(Z,Y)))).

fof(goal, conjecture, ! [X,Y] : (q(X,Y) <=> q(Y,X))).
