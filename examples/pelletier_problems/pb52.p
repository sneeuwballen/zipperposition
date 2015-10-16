
fof(ax1, axiom, ? [Z,W] : ! [X,Y] :
  (f(X,Y) <=> (X=Z & Y=W))).

fof(goal, conjecture, ? [W] : ! [Y] : (
  (? [Z] : ! [X] : (f(X,Y) <=> X=Z)) <=>
  Y=W)
).
