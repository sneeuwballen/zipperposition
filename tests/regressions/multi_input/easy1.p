% easy UNSAT problem: p and ~p, conjecture false
fof(p, axiom, p).
fof(np, axiom, ~p).
fof(goal, conjecture, $false).
