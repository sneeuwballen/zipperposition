
fof(ax1, axiom, ! [X] :
  (   (f(X) &
      ! [Y] : ((g(Y) & h(X,Y)) => j(X,Y)))
  => ! [Y] : (((g(Y) & h(X,Y)) => k(Y))))
).

fof(ax2, axiom, ~ ? [Y] : (l(Y) & k(Y))).

fof(ax3, axiom, ? [X] :
  (f(X) &
   ! [Y] : (h(X,Y) => l(Y)) &
   ! [Y] : ((g(Y) & h(X,Y)) => j(X,Y))
  )
).

fof(goal, conjecture, ? [X] :
  (f(X) & 
   (~ (? [Y] : (g(Y) & h(X,Y)))))
).
