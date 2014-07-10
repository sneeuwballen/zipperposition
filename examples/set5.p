% This example can be found in the article "The Strive-based Subset Prover",
% page 205, example 5.
tff(ty_a, type, a: !>[A:$tType]: set(A)).
tff(ty_b, type, b: !>[A:$tType]: set(A)).
tff(1, conjecture,
  (?[X:set(A),Y:set(A)]:
    (![Z:A]:
      (member(A,Z,union(A,a(A),b(A))) => member(A,Z,intersection(A,X,Y)))
        )
    )
).
