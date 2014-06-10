% This example can be found in the article "The Strive-based Subset Prover",
% page 205, example 5.
fof(1, conjecture,
    ![S0:set(A),S1:set(A)]:
(? [X:set(A),Y:set(A)]:
          (![Z:A]:
            (member(A,Z,union(A,S0,S1)) => member(A,Z,intersection(A,X,Y)))
        )
    )
).
