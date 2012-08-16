
% hard one!

include('ax_logic_a.ax').
include('ax_logic_b.ax').

fof(ax_logic_c_bis, axiom, ! [X,Y] : t(i(i(Y,X),i(n(X),n(Y))))).

include('ax_logic_d.ax').

fof(goal, conjecture, ! [X] : t(i(X,n(n(X))))).

