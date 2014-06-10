% This example can be found in the article "The Strive-based Subset Prover",
% page 204, example 2.
fof(1, conjecture,
    ![X:A,Y:A,S:set(A)]:
        ((member(A,X,S) & ~ member(A,Y,S)) => ? [S2:set(A)]:(member(A,X,difference(A,S2,singleton(A,Y)))))
).
