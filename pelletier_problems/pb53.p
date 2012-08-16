
fof(ax1, axiom, ? [X,Y] : (
  X != Y &
  ! [Z] : (Z = X | Z = Y))).


fof(goal, conjecture,
  (? [Z] : ! [X] :
    ((? [W] : ! [Y] : (f(X,Y) <=> Y = W))
    <=> X = Z))
  <=>
  (? [W] : ! [Y] :
    ((? [Z] : ! [X] : (f(X,Y) <=> X = Z))
    <=> Y = W))
  ).
