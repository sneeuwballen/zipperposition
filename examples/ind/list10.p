
% from paper on CVC4:  sum(rev(L)) = sum(L)

include('nat.ax').
include('list.ax').

tff(ty_sum, type, sum : list > nat).

tff(sum1, axiom, sum(nil) = z).
tff(sum2, axiom, ![N:nat, L:list]: sum(cons(N,L)) = plus(N, sum(L))).

tff(the, conjecture,  ![L:list]: sum(rev(L)) = sum(L)).
