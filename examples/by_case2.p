
% let's try to make a more interesting case analysis

tff(ty_a, type, a : $int).
tff(ty_t, type, t : $int).
tff(ty_p, type, p : $int > $o).

tff(1, axiom, $lesseq(a, t)).
tff(2, axiom, $lesseq(t, $sum(a,2))).
tff(3, axiom, p(a) & p($sum(a,1)) & p($sum(a,2))).
tff(4, conjecture, p(t)).
