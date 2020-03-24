
tff(list_ind, type, list : $tType, inductive(cons, nil)).

tff(ty_cons, type, cons : ($i * list) > list).
tff(ty_nil, type, nil : list).

tff(ty_append, type, append : (list * list) > list).
tff(ty_length, type, length : list > $int).

tff(length_1, axiom, length(nil) = 0).
tff(length_2, axiom, ![X:$i, L:list]: length(cons(X,L)) = $sum(1, length(L))).

tff(append_1, axiom, ![L:list]: append(nil,L) = L).
tff(append_2, axiom, ![X:$i, L:list, L2:list]:
    append(cons(X,L), L2) = cons(X,append(L,L2))).


tff(length_1, axiom, length(nil) = 0).
tff(length_2, axiom, ![X:$i, L:list]: length(cons(X,L)) = $sum(1, length(L))).

tff(the, conjecture,
    ![L1:list, L2:list]:
        ( length(append(L1, L2)) = $sum(length(L1), length(L2)) )
    ).
