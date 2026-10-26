tff(a_type, type, a: $tType).
tff(c1_type, type, c1: a).
tff(c2_type, type, c2: a).
tff(c_distinct, axiom, c1 != c2).
tff(a_enum, axiom, ! [X: a] : (X = c1 | X = c2)).
tff(goal, conjecture, ? [X: a] : (X != c1)).
