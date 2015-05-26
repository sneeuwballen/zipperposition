% X ≤ Y => X+Z ≤ Y+Z

include('nat.ax').

tff(the, conjecture,
    ![X:nat, Y:nat, Z:nat]:
        (leq(X,Y) => leq(plus(X,Z), plus(Y,Z)))).


