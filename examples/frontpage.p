
% induction on lists

tff(list_ind, type, list : $tType, inductive(cons, nil)).

tff(ty_cons, type, cons : ($i * list) > list).
tff(ty_nil, type, nil : list).

tff(ty_append, type, append : (list * list) > list).
tff(ty_length, type, length : list > $int).
tff(ty_dup, type, dup : list > list).
tff(ty_rev, type, rev : list > list).
tff(ty_rev_append, type, rev_append : (list * list) > list).

tff(length_1, axiom, length(nil) = 0).
tff(length_2, axiom, ![X:$i, L:list]: length(cons(X,L)) = $sum(1, length(L))).

tff(append_1, axiom, ![L:list]: append(nil,L) = L).
tff(append_2, axiom, ![X:$i, L:list, L2:list]:
    append(cons(X,L), L2) = cons(X,append(L,L2))).

tff(dup_1, axiom, dup(nil) = nil).
tff(dup_2, axiom, ![X:$i, L:list]: dup(cons(X,L)) = cons(X,cons(X,dup(L)))).

tff(rev, axiom, ![L:list]: rev(L) = rev_append(nil,L)).
tff(rev_1, axiom, ![L:list]: rev_append(L, nil) = L).
tff(rev_2, axiom, ![X:$i, L1:list, L2:list]:
    rev_append(L1, cons(X,L2)) = rev_append(cons(X,L1), L2)).

% works

tff(the, conjecture,
    ![L:list]:
        length(dup(L)) = $product(2, length(L))
).

% do not work

%tff(the, conjecture,
%    ![L:list]: length(rev(dup(L))) = $product(2,length(L))
%    ).

%tff(the, conjecture,
%    ![L:list]: length(rev(L)) = length(L)
%    ).

%tff(the, conjecture,
%    ![L1:list, L2:list]:
%        ( length(append(L1, L2)) = $sum(length(L1), length(L2)) )
%    ).

% NOTE: to run it,
% zipperposition -arith -theory src/builtin.theory examples/frontpage.p -arith-inf-diff-to-lesseq
%
% possible option  -meta-summary
