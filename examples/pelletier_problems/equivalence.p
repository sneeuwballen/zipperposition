% example problem in the paper "delayed equivalence reasoning in superposition"

fof(a1, axiom, ![A,B]: (equ(A, B) <=> ![C]: (contains(A, C) <=> contains(B, C)))).

fof(goal, conjecture, ! [A, B, C]: ((equ(C, B) & equ(B, A)) => equ(C, A))).
