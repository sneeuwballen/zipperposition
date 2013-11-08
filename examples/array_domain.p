
% Proof of lemma on finite domain partial functions

% map
tff(ty_selected, type, select : (map(A,B) * A) > B).
tff(ty_update, type, update : (map(A, B) * A * B) > map(A,B)).
tff(ty_empty, type, empty_map : map(A,B)).
tff(ty_mem_map, type, mem_map : (A * map(A,B)) > $o).
tff(map1, axiom, ![M: map(A,B), K:A, V:B]: select(update(M, K, V), K) = V).
tff(map2, axiom, ![M: map(A,B), K:A, K2:A, V:B]: (K != K2 => select(update(M,K,V), K2) = select(M, K2))).
tff(map3, axiom, ![K:A]: ~mem_map(K, empty_map)).
tff(map5_1, axiom, ![M: map(A,B), K:A, V:B, K2:A]:
    (mem_map(K, update(M, K2, V)) <=>
        ( K = K2
        | (K != K2 & mem_map(K, M))))).

tff(map6, axiom, ![M: map(A,B)]:
    ( M = empty_map
    | ?[M2: map(A,B), K:A, V:B]: M=update(M2, K, V))).
tff(map7, axiom,
    ![M: map(A,B), K:A, V:B]: update(M,K,V) != empty_map).

% list TODO
tff(ty_nil, type, nil : list(A)).
tff(ty_cons, type, cons : (A * list(A)) > list(A)).
tff(ty_mem_list, type, mem_list : (A * list(A)) > $o).
tff(list3_1, axiom, ![X:A, Y:A, L:list(A)]:
    (mem_list(X, cons(Y,L)) <=>
        ( X = Y
        | (X != Y & mem_list(X,L))))).

tff(list4, axiom, ![L:list(A)]: (L = nil | ?[L2:list(A), X:A]: L=cons(X,L2))).
tff(list5, axiom, ![L:list(A), X:A]: cons(X,L) != nil).

% goal: L subset dom(F) => x::L subset dom(F + x -> y)

tff(g, conjecture,
    ![F:map(A,B), X:A, Y:B, L:list(A)]:
        (
            (![Z:A]: (mem_list(Z,L) => mem_map(Z,F))) =>
            (![Z:A]: (mem_list(Z,cons(X,L)) => mem_map(Z,update(F,X,Y))))
        )
    ).
