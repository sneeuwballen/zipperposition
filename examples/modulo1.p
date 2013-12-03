
% 2 a + 2 mod 3 = 0  ==> a +1 mod 3 = 0  ==> a mod 3 = 2
% slightly harder variation on modulo3.p

tff(ty_a, type, a : $int).
tff(1, axiom, $remainder_e($sum($product(a, 2), +2), 3) = 0).
tff(the, conjecture, $remainder_e(a, 3) = 2).
