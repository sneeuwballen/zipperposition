
% associativity

%include('nat.ax').
tff(nat_ind, type, nat : $tType, inductive(s, z)).

tff(ty_s, type, s:nat > nat).
tff(ty_z, type, z:nat).
tff(ty_plus, type, plus : (nat*nat) > nat).

% tff(test, axiom, ![X:nat]: s(X) != z).

tff(plus_1, axiom, ![X:nat]: plus(z,X) = X).
tff(plus_2, axiom, ![X:nat, Y:nat]: plus(s(X),Y) = s(plus(X,Y))).


tff(1, conjecture, ![X:nat, Y:nat, Z:nat]: plus(X,plus(Y,Z)) = plus(plus(X,Y),Z)).
