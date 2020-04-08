
fof(ax1, axiom, ? [Z,W] : ! [X,Y] :
  (f(X,Y) <=> (X=Z & Y=W))).

fof(goal, conjecture, ? [Z] : ! [X] : (
  (? [W] : ! [Y] : (f(X,Y) <=> Y=W)) <=>
  X=Z)
).
