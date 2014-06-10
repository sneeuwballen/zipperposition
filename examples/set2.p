% This example can be found in the article "The Strive-based Subset Prover",
% page 204, example 1.
fof(1, conjecture,
    ![X:set(A),Y:set(A)]:
        (![Z:A]:(member(A,Z,X) <=> member(A,Z,Y)) => X = Y)
).
