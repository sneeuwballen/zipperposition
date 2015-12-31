% double(X) = X+X


% declare that "nat" is an inductive type
tff(nat_ind, type, nat : $tType, inductive(s, z)).
tff(ty_s, type, s:nat > nat).
tff(ty_z, type, z:nat).
tff(ty_plus, type, plus : (nat*nat) > nat).

tff(ty_double, type, double : nat > nat).

tff(plus_1, axiom, ![X:nat]: plus(z,X) = X).
tff(plus_2, axiom, ![X:nat, Y:nat]: plus(s(X),Y) = s(plus(X,Y))).

tff(double1, axiom, ![X:nat]: double(s(X)) = s(s(double(X)))).
tff(double2, axiom, double(z) = z).

tff(the, conjecture, ![X:nat]: double(X) = plus(X,X)).
