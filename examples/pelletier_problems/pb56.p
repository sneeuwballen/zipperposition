
fof(goal, conjecture,
  ! [X] : (
    (? [Y] : (f(Y) & X = g(Y)))
    =>
    f(X)
  )
<=>
  ! [X] : (f(X) => f(g(X)))
).
