
tff(ty_selected, type, select : !>[A:$tType, B:$tType]: (map(A,B) * A) > B).
tff(ty_update, type, update : !>[A:$tType, B:$tType]: (map(A, B) * A * B) > map(A,B)).
tff(ty_empty, type, empty_map : !>[A:$tType, B:$tType]: map(A,B)).
tff(1, axiom, ![A:$tType, B:$tType, M: map(A,B), K:A, V:B]:
    select($_, $_, update($_, $_, M, K, V), K) = V).
tff(2, axiom, ![A:$tType, B:$tType, M: map(A,B), K:A, K2:A, V:B]:
    (K != K2 => select($_, $_, update(A,B, M,K,V), K2) = select($_, $_, M, K2))).
tff(3, hypothesis, k1 != k2).
tff(4, conjecture, select($_, $_, update($_, $_, update($_, $_, empty_map($_, $_), k1, a), k2, b), k1) = a).
