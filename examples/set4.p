% This example can be found in the article "The Strive-based Subset Prover",
% page 205, example 4.
fof(1, conjecture,
    ![X:set(A),S0:set(A),S1:set(A)]:
        ((~ subseteq(A,S0,X) => ~ subseteq(A,S1,X)) => subseteq(A,S0,S1))
).
