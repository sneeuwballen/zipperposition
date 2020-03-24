
% declinaison of by_case2

tff(ty_a, type, a : $int).
tff(ty_u, type, u : $int).
tff(ty_t, type, t : $int).
tff(ty_p, type, p : $int > $o).

tff(1, axiom, $lesseq($sum(a,u), t)).
tff(2, axiom, $lesseq($difference(t,u), $sum(a,2))).
tff(3, axiom, p($sum(a,u)) & p($sum($sum(a,u),1)) & p($sum($sum(a,u),2))).
tff(4, conjecture, p(t)).
