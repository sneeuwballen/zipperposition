

% not theorem, bad retranscription?

fof(ax1, axiom, ! [Y] : ? [Z] : ! [X] : (f(X,Z) <=> X = Y)).

fof(goal, conjecture, ~ (
  ? [W] : ! [X] : (
    f(X,W) <=> (! [U] : (
      f(X,U) => (? [Y] : (f(Y,U) & ~ (? [Z] : (f(X,U) & f(Z,Y)))))
    ))
  )
)).
