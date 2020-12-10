
fof(ax1, axiom, ! [X] :
  ((f(X) &
   ! [Y] : ((f(Y) & h(X,Y)) => g(Y)))
  => g(X))
).

fof(ax2, axiom,
  (? [X] : (f(X) & ~ g(X))) =>
  (? [X] : (f(X) & ~ g(X) &
            ! [Y] : ((f(Y) & ~g(Y)) => j(X,Y))))
).

fof(ax3, axiom, ! [X,Y] :
  (  (f(X) & f(Y) & h(X,Y))
  => ~ j(X,Y))
).

fof(goal, conjecture, ! [X] : (f(X) => g(X))).
