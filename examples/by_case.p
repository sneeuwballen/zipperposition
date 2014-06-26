
% thanks to Andrei and Jean-Christophe for this simple example

tff(0, type, p : $int > $o).
tff(1, axiom, p(0) & p(1)).
tff(2, conjecture,
  ![X:$int]:
    (($lesseq(0,X) &
     $lesseq(X,1)) =>
     p(X))).
