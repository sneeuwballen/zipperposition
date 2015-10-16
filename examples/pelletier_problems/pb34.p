
% Andrew`s Challenge

fof(goal, conjecture,
  ((? [X] : ! [Y] : (p(X) <=> p(Y))) <=>
   ((? [X] : q(X)) <=> ! [Y] : p(Y)))
  <=>
  ((? [X] : ! [Y] : (q(X) <=> q(Y))) <=>
   ((? [X] : p(X)) <=> ! [Y] : q(Y)))
).
