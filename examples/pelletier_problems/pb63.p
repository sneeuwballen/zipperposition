
include('ax_a.ax').
include('ax_b.ax').
include('ax_c.ax').

fof(goal, conjecture, ! [X,Y,Z] : (f(X,Y) = f(Z,Y) => X = Z)).
