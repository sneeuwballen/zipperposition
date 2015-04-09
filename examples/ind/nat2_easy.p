
% commutativity

include('nat.ax').

% helper
tff(yolo, axiom, ![X:nat, Y:nat]: plus(X,s(Y)) = s(plus(X,Y))).

tff(1, conjecture, ![X:nat, Y:nat]: plus(X,Y)=plus(Y,X)).
