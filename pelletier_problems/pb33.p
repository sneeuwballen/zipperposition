
fof(goal, conjecture,
  ! [X] : ((p(a) & (p(X) => p(b))) => p(c)) <=>
  ! [X] : ((~ p(a) | p(X) | p(c)) &
           (~ p(a) | ~ p(b) | p(c)))
).
