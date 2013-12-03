
% 2 a mod 3 = 1  ==> a mod 3 = 1

tff(ty_a, type, a : $int).
tff(1, axiom, $remainder_e($sum($product(a, 2), -1), 3) = 0).
tff(the, conjecture, $remainder_e(a, 3) = 1).
