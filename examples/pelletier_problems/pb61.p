
fof(ax1, axiom, ! [X,Y,Z] :
  f(X, f(Y,Z)) = f(f(X,Y),Z)).

fof(goal, conjecture, ! [X,Y,Z,W] :
  f(X,f(Y,f(Z,W))) = f(f(f(X,Y),Z),W)).
