% abelian group
sum X zero --> X.
sum X (minus X) --> zero.
minus zero --> zero.
minus (minus X) --> X.
minus (sum X Y) --> sum (minus X) (minus Y). % abelian group

prod X (sum Y Z) --> sum (prod X Y) (prod X Z).
prod (sum X Y) Z --> sum (prod X Z) (prod Y Z).
prod X zero --> zero.
prod X (minus Y) --> minus (prod X Y).
prod zero X --> zero.
prod (minus X) Y --> minus (prod X Y).


% vim:syntax=prolog
