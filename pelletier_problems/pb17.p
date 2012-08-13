
fof(goal, conjecture,
  ((p & (q => r)) => s) <=>
  ((~ p | q | s) &
   (~ p | ~ r | s))).
