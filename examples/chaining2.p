
tff(t1, type, a : $int).
tff(t2, type, b : $int).
tff(t3, type, c : $int).
tff(t4, type, d : $int).
tff(tf, type, (f : $int > $int)).

tff(1, axiom, $less(f(a), f(b))).
tff(2, axiom, $less(f(b), f(c))).
tff(3, axiom, $less(f(c), f(d))).
tff(4, conjecture, ?[X:$int]: $less(f(X), f(d))).
