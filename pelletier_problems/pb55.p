
% who killed aunt Agatha?

fof(ax1, axiom, ? [X] : (l(X) & k(X,a))).

fof(ax2, axiom, l(a) & l(b) & l(c)).

fof(ax3, axiom, ! [X] : (
  l(X) => (X = a | X = b | X = c))
).

fof(ax4, axiom, ! [Y,X] : (k(X,Y) => h(X,Y))).

fof(ax5, axiom, ! [X,Y] : (k(X,Y) => ~ r(X,Y))).

fof(ax6, axiom, ! [X] : (h(a,X) => ~ h(c,X))).

fof(ax7, axiom, ! [X] : (X != b => h(a,X))).

fof(ax8, axiom, ! [X] : ((~ r(X,a)) => h(b,X))).

fof(ax9, axiom, ! [X] : (h(a,X) => h(b,X))).

fof(ax10, axiom, ! [X] : ? [Y] : ~ h(X,Y)).

fof(ax11, axiom, a != b).

fof(goal, conjecture, k(a,a)).
