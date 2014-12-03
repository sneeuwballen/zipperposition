
% use  -induction nat:z|s

tff(ty_s, type, s:nat > nat).
tff(ty_z, type, z:nat).

tff(ty_plus, type, plus:(nat*nat)>nat).
tff(ty_mult, type, mult:(nat*nat)>nat).

tff(1, axiom, ![X:nat,Y:nat]: plus(s(X),Y) = s(plus(X,Y))).
tff(2, axiom, ![X:nat]: plus(z,X) = X).

tff(the, conjecture, ![X:nat,Y:nat]: plus(X,Y) = plus(Y,X)).
