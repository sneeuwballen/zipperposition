
fof(ax1, axiom, ! [X] :
  (f(X) => (
    (? [Y] : (g(Y) & h(X,Y))) &
    (? [Y] : (g(Y) & ~ h(X,Y))))
  )
).

fof(ax2, axiom, ? [X] : (j(X) & ! [Y] : (g(Y) => h(X,Y)))).

fof(goal, conjecture, ? [X] : (j(X) & ~ f(X))).
