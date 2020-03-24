mult X e --> X.
mult e X --> X.
mult X (inv X) --> e.
mult (inv X) X --> e.
inv e --> e.
inv (inv X) --> X.
inv (mult X Y) --> mult (inv Y) (inv X).
mult (mult X Y) Z --> mult X (mult Y Z).
mult X (mult (inv X) Y) --> Y.
mult (inv X) (mult X Y) --> Y.

% vim:syntax=prolog
