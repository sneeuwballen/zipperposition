
% not a theorem (SPASS finds a completion)
% expect: sat

fof(goal, conjecture,
  (f(a) & ! [X] : (f(X) => f(g(X))))
  <=>
  ! [X] : (
    (~ f(a) | f(X) | f(g(g(X)))) &
    (~ f(a) | ~ f(g(X)) | f(g(g(X))))
  )
).
