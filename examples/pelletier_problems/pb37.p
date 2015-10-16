
fof(ax1, axiom, ! [Z] : ? [W] : ! [X] : ? [Y] :
  ((p(X,Z) => p(Y,W)) &
   p(Y,Z) &
   (p(Y,W) => ? [U] : q(U,W)))).

fof(ax2, axiom, ! [X,Z] : (~ p(X,Z) => ? [Y] : q(Y,Z))).

fof(ax3, axiom, ? [X,Y] : q(X,Y) => ! [X] : r(X,X)).

fof(goal, conjecture, ! [X] : ? [Y] : r(X,Y)).
