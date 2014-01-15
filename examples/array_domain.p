
% Proof of lemma on finite domain partial functions

% map
tff(ty_selected, type, select : !>[A:$tType, B:$tType]: (map(A,B) * A) > B).
tff(ty_update, type, update : !>[A:$tType, B:$tType]: (map(A, B) * A * B) > map(A,B)).
tff(ty_empty, type, empty_map : !>[A:$tType, B:$tType]: map(A,B)).
tff(ty_mem_map, type, mem_map : !>[A:$tType, B:$tType]: (A * map(A,B)) > $o).
tff(map1, axiom, ![A:$tType, B:$tType, M: map(A,B), K:A, V:B]: select(A,B,update(A,B, M, K, V), K) = V).
tff(map2, axiom, ![A:$tType, B:$tType, M: map(A,B), K:A, K2:A, V:B]:
    (K != K2 => select($_, $_, update($_, $_, M,K,V), K2) = select($_, $_, M, K2))).
tff(map3, axiom, ![A:$tType, K:A]: ~mem_map($_, $_, K, empty_map($_, $_))).
tff(map5_1, axiom, ![A:$tType, B:$tType, M: map(A,B), K:A, V:B, K2:A]:
    (mem_map(A, B, K, update($_, $_, M, K2, V)) <=>
        ( K = K2
        | (K != K2 & mem_map($_, B, K, M))))).

tff(map6, axiom, ![A:$tType, B:$tType, M: map(A,B)]:
    ( M = empty_map(A, B)
    | ?[M2: map(A,B), K:A, V:B]: M=update($_, $_, M2, K, V))).
tff(map7, axiom,
    ![A:$tType, B:$tType, M: map(A,B), K:A, V:B]:
        update(A,B,M,K,V) != empty_map($_,$_)).

% list
tff(ty_nil, type, nil : !>[A:$tType]: list(A)).
tff(ty_cons, type, cons : !>[A:$tType]: (A * list(A)) > list(A)).
tff(ty_mem_list, type, mem_list : !>[A:$tType]: (A * list(A)) > $o).
tff(list3_1, axiom, ![A:$tType, X:A, Y:A, L:list(A)]:
    (mem_list(A, X, cons(A,Y,L)) <=>
        ( X = Y
        | (X != Y & mem_list(A,X,L))))).

tff(list4, axiom, ![A:$tType, L:list(A)]:
    (L = nil(A) | ?[L2:list(A), X:A]: L=cons(A,X,L2))).

tff(list5, axiom, ![A:$tType, L:list(A), X:A]: cons(A,X,L) != nil(A)).

% goal: L subset dom(F) => x::L subset dom(F + x -> y)

tff(g, conjecture,
    ![A:$tType, B:$tType, F:map(A,B), X:A, Y:B, L:list(A)]:
        (
            (![Z:A]: (mem_list(A,Z,L) => mem_map(A,B,Z,F))) =>
            (![Z:A]: (mem_list(A,Z,cons(A,X,L)) => mem_map(A,B,Z,update(A,B,F,X,Y))))
        )
    ).
