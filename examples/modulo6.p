
% harder problem: 3|a & a=2b => a mod 6 = 0 

tff(ty_a, type, a : $int).
tff(ty_divisible, type, divisible : ($int * $int) > $o).

tff(1, axiom, ![X:$int, Y:$int]: (divisible(X, Y) <=> $remainder_e(X,Y) = 0)).
tff(2, axiom, divisible(a, 3)).
tff(3, axiom, ?[X:$int]: (a = $product(2,X))).
tff(the, conjecture, $remainder_e(a, 6) = 0).
