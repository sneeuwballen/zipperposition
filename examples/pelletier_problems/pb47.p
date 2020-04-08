
% Schubert`s steamroller

fof(ax1, axiom, ! [X] : (p1(X) => p0(X)) & ? [X] : p1(X)).
fof(ax2, axiom, ! [X] : (p2(X) => p0(X)) & ? [X] : p2(X)).
fof(ax3, axiom, ! [X] : (p3(X) => p0(X)) & ? [X] : p3(X)).
fof(ax4, axiom, ! [X] : (p4(X) => p0(X)) & ? [X] : p4(X)).
fof(ax5, axiom, ! [X] : (p5(X) => p0(X)) & ? [X] : p5(X)).

fof(ax6, axiom, ? [X] : q1(X) & ! [X] : (q1(X) => q0(X))).

fof(ax7, axiom, ! [X] :
  (p0(X) =>
   ((! [Y] : (q0(Y) => r(X,Y))) |
    (! [Y] : ((p0(Y) & s(Y,X) &
               ? [Z] : (q0(Z) & r(Y,Z)))
              => r(X,Y)
             )
    )))
).

fof(ax8, axiom, ! [X,Y] :
  ((p3(Y) & (p5(X) | p4(X))) =>
   s(X,Y))
).

fof(ax9, axiom, ! [X,Y] : ((p3(X) & p2(Y)) => s(X,Y))).
fof(ax10, axiom, ! [X,Y] : ((p2(X) & p1(Y)) => s(X,Y))).
fof(ax11, axiom, ! [X,Y] : (
  (p1(X) & (p2(Y) | q1(Y))) => ~ r(X,Y))
).
fof(ax12, axiom, ! [X,Y] : ((p3(X) & p4(Y)) => r(X,Y))).
fof(ax13, axiom, ! [X,Y] : ((p3(X) & p5(Y)) => ~ r(X,Y))).

fof(ax14, axiom, ! [X] : ((p4(X) | p5(X)) =>
  ? [Y] : (q0(Y) & r(X,Y)))
).

fof(goal, conjecture, ? [X,Y] : (p0(X) & p0(Y) &
  ? [Z] : (q1(Z) & r(Y,Z) & r(X,Y)))
).
