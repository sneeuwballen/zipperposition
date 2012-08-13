
% this should be a theorem, but SPASS finds a completion...

fof(ax1, axiom, (! [X] : p(X)) => (! [X] : q(X))).

fof(ax2, axiom, (! [X] : (q(X) | r(X))) =>
  (? [X] : (q(X) & s(X)))).

fof(ax3, axiom, (? [X] : s(X)) => (! [X] : (f(X) => g(X)))).

fof(goal, conjecture, ! [X] : ((p(X) & f(X)) => g(X))).
