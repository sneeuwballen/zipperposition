

include('ax_a.ax').
include('ax_b.ax').
include('ax_c.ax').

fof(goal, conjecture, ! [X,Y] : (f(Y,X) = a => f(X,Y) = a)).
