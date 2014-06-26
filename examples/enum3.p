
tff(ty_nil,type, nil: !>[A:$tType]: list(A)).
tff(ty_cons,type, cons: !>[A:$tType]: (A * list(A)) > list(A)).
tff(ty_f,type, f : !>[A:$tType]: A > $i).
tff(ty_mapf,type, map_f : !>[A:$tType]: list(A) > list($i)).
tff(0,axiom, ![L:list(A)]: (L = nil(A) | (?[X:A, L2:list(A)]: L=cons(A,X,L2)))).
% tff(1,axiom, ![X:A,L:list(A)]: (map_f(A, nil(A)) = nil($i) & map_f(A, cons(A,X,L)) = cons($i,f(A,X),map_f(A,L)))).
tff(2,axiom, ![X:A,L:list(A)]: (nil(A) != cons(A,X,L))).
