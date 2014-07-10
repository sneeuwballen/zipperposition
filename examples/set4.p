% This example can be found in the article "The Strive-based Subset Prover",
% page 205, example 4.

tff(ty_a, type, a: !>[A:$tType]: set(A)).
tff(ty_b, type, b: !>[A:$tType]: set(A)).

tff(1, conjecture,
        (![X:set(A)]:(~ subseteq(A,a(A),X) => ~ subseteq(A,b(A),X))) => subseteq(A,a(A),b(A))
).
