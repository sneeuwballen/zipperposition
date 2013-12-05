
tff(ty_selected, type, select : (map(A,B) * A) > B).
tff(ty_update, type, update : (map(A, B) * A * B) > map(A,B)).
tff(ty_empty, type, empty_map : map(A,B)).
tff(ty_k1, type, k1 : list(A)).
tff(ty_k2, type, k2 : list(A)).
tff(ty_a, type, a : array($int)).
tff(1, axiom, ![M: map(A,B), K:A, V:B]: select(update(M, K, V), K) = V).
tff(2, axiom, ![M: map(A,B), K:A, K2:A, V:B]: (K != K2 => select(update(M,K,V), K2) = select(M, K2))).
tff(3, hypothesis, k1 != k2).
tff(4, conjecture, select(update(update(empty_map, k1, a), k2, b), k1) = a).

