
fof(goal, conjecture, ! [X] : (
  f(X,g(X))
  <=> ? [Y] : (
    ! [Z] : (f(Z,Y) => f(Z, g(X)))
    & f(X,Y)
  
  ))).
