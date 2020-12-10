
% a mod 4 = 1 ==> a mod 2 = 1

tff(ty_a, type, a : $int).
tff(1, axiom, $remainder_e(a, 4) = 1).
tff(the, conjecture, $remainder_e(a, 2) = 1).
