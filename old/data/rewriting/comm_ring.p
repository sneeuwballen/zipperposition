% Commutative rings

val zero : carrier.
val plus : carrier -> carrier -> carrier.
val minus : carrier -> carrier.
val mult : carrier -> carrier -> carrier.

plus X (minus X) --> zero.
plus X zero --> X.
minus (minus X) --> X.
minus zero --> zero.
minus (plus X Y) --> plus (minus X) (minus Y).
mult X one --> X.
mult X zero --> zero.
mult X (plus Y Z) --> plus (mult X Y) (mult X Z).
mult X (minus Y) --> minus (mult X Y).


% vim:syntax=prolog


