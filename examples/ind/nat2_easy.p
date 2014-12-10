
% commutativity

%include('nat.ax').

tff(ty_s, type, s:nat > nat).
tff(ty_z, type, z:nat).
tff(ty_plus, type, plus : (nat*nat) > nat).
tff(ty_minus, type, minus : (nat*nat) > nat).
tff(plus_1, axiom, ![X:nat]: plus(z,X) = X).
tff(plus_2, axiom, ![X:nat, Y:nat]: plus(s(X),Y) = s(plus(X,Y))).

% helper
tff(yolo, axiom, ![X:nat, Y:nat]: plus(X,s(Y)) = s(plus(X,Y))).

tff(1, conjecture, ![X:nat, Y:nat]: plus(X,Y)=plus(Y,X)).
