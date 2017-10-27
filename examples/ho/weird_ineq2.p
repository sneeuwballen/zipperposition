% expect: unsat

thf(a1,axiom, ?[X:$i>$o] : ?[Y:$i>$o] : ?[Z:$i>$o] :
    (
      (~ (![U:($i>$o)>$o] :  ((U@X) => (U@Y)))) &
      (~ (![U:($i>$o)>$o] :  ((U@X) => (U@Z)))) &
      (~ (![U:($i>$o)>$o] :  ((U@Y) => (U@Z))))
    )
).

thf(c1,conjecture,?[X:$i>$o]: ?[A:$i] : ?[B:$i] : ((X @ A) & ~(X @ B)) ).
