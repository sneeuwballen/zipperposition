
% typical loop invariant conservation property

tff(ty1, type, k: $int).
tff(ty2, type, p: $int > $o).

tff(1, axiom, ![I: $int]: (($lesseq(0,I) & $lesseq(I,k)) => p(I))).
tff(2, axiom, p($sum(k,1))).
tff(3, conjecture, ![I: $int]: (($lesseq(0,I) & $lesseq(I,$sum(k,1))) => p(I))).
