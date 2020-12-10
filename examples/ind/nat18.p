% X ≤ Y => Z+X ≤ Z+Y

include('nat.ax').

tff(the, conjecture,
    ![X:nat, Y:nat, Z:nat]:
        (leq(X,Y) => leq(plus(Z,X), plus(Z,Y)))).

