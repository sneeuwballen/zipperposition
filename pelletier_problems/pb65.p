
include('ax_a.ax').
include('ax_b.ax').

fof(goal, conjecture,
  (! [X] : f(X,X) = a)
  =>
  (! [X,Y] : f(X,Y) = f(Y,X))
).

