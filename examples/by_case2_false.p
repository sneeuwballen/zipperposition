
% false version of by_case2

tff(ty_a, type, a : $int).
tff(ty_t, type, t : $int).
tff(ty_p, type, p : $int > $o).

tff(1, axiom, $lesseq(a, t)).
tff(2, axiom, $lesseq(t, $sum(a,3))).
tff(3, axiom, p(a) & p($sum(a,1)) & p($sum(a,2))).
tff(4, conjecture, p(t)).
