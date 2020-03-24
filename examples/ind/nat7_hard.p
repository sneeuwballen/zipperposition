
% Kersani & Peltier, commutativity of n-ary addition

%include('nat.ax').
tff(nat_ind, type, nat : $tType, inductive(s, z)).

tff(ty_s, type, s:nat > nat).
tff(ty_z, type, z:nat).
tff(ty_plus, type, plus : (nat*nat) > nat).
tff(plus_1, axiom, ![X:nat]: plus(z,X) = X).
tff(plus_2, axiom, ![X:nat, Y:nat]: plus(s(X),Y) = s(plus(X,Y))).

% series of integer numbers
tff(ty_x, type, x : nat > nat).

% p(n): sum of x1, ..., xn
tff(ty_p, type, p : nat > nat).

tff(p1, axiom, p(z) = z).
tff(p2, axiom, ![N:nat]: p(s(N)) = plus(p(N), x(N))).

% q(n): sum of xn, ..., x1
tff(ty_q, type, q : nat > nat).

tff(q1, axiom, q(z) = z).
tff(q2, axiom, ![N:nat]: q(s(N)) = plus(x(N), q(N))).

% AC of +: to be deduced!

tff(the, conjecture, ![N:nat]: p(N) = q(N)).
