% easy UNSAT problem: q(a) and ~q(a), conjecture false
fof(q, axiom, q(a)).
fof(nq, axiom, ~q(a)).
fof(goal, conjecture, $false).
