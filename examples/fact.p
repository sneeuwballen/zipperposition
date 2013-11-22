
tff(ty_fact, type, fact : $int > $int).
tff(1, axiom, fact(0) = 1).
tff(2, axiom, ![N:$int]: ($lesseq(1, N) => fact(N) = $product(fact($difference(N,1)), N))).
tff(3, conjecture, fact(5) = 120).
