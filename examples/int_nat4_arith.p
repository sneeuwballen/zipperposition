% encoding ind/nat4.p into arith

tff(ty_double, type, double : $int > $int).

tff(double1, axiom, ![X:$int]: ($greater(X, 0) => double($sum(1,X)) = $sum(2, double(X)))).
tff(double2, axiom, double(0) = 0).

tff(the, conjecture,
  ![X:$int]: ($greatereq(X,0) => double(X) = $sum(X,X))).
