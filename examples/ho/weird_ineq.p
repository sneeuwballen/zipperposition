
% expect: sat

% example from Martin

thf(a1,axiom, ?[X:$i>$o] : ?[Y:$i>$o] : ?[Z:$i>$o] :
  ~ (![U:($i>$o)>$o] : ((U@X) => (U@Y))) ).

thf(c1,conjecture,?[X:$i>$o]: ?[A:$i] : ?[B:$i] : ((X @ A) & ~(X @ B)) ).
