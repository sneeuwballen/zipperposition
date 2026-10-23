tff(a_type, type, a: $tType).
tff(d1_type, type, d1: a).
tff(d2_type, type, d2: a).
tff(d_distinct, axiom, d1 != d2).
tff(a_enum, axiom, ! [X: a] : (X = d1 | X = d2)).
tff(goal, conjecture, ? [X: a] : (X != d1)).
