
% test consistency of theory

include('nat.ax').

tff(ty_p, type, p:(nat * nat) > $o).
tff(ty_a, type, a:nat).
tff(ty_b, type, b:nat).

% a minimal model
%tff(axiom_a, axiom, a=z).
%tff(axiom_b, axiom, b=s(z)).

tff(axiom_1, axiom, ![X:nat, Y:nat]: (p(X,Y) => s(X)=Y)).

tff(axiom_2, axiom, p(a,b)).
% similar to conjecture  ![X,Y]: ~p(X,Y)  which is false

