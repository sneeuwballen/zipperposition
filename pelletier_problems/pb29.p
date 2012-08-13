
fof(ax1, axiom, ? [X] : f(X) & ? [X] : g(X)).

fof(goal, conjecture,
  ( ! [X] : (f(X) => h(X)) &
    ! [X] : (g(X) => j(X))) <=>
  ( ! [X,Y] : ((f(X) & g(Y)) => (h(X) & j(Y))))
).
