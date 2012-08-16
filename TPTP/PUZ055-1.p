%--------------------------------------------------------------------------
% File     : PUZ055-1 : TPTP v5.2.0. Released v2.7.0.
% Domain   : Puzzles
% Problem  : Show that Sam Loyd's fifteen-puzzle is not solvable
% Version  : [Cla03] axioms : Especial.
% English  : The classic fifteen-puzzle with the 14 and 15 interchanged.
%            Show that it is not solvable by showing satisfiability of
%            the following problem.

% Refs     : [Cla03] Claessen (2003), Email to G. Sutcliffe
% Source   : [Cla03]
% Names    :

% Status   : Unknown
% Rating   : 1.00 v2.7.0
% Syntax   : Number of clauses     :    4 (   1 non-Horn;   3 unit;   2 RR)
%            Number of atoms       :    5 (   3 equality)
%            Maximal clause size   :    2 (   1 average)
%            Number of predicates  :    2 (   0 propositional; 1-2 arity)
%            Number of functors    :   19 (  18 constant; 0-2 arity)
%            Number of variables   :    8 (   0 singleton)
%            Maximal term depth    :   21 (   8 average)
% SPC      : CNF_UNK_NUE

% Comments : Should be satisfiable.
%--------------------------------------------------------------------------
cnf(initial_state,axiom,
    ( state(l(n1,l(n2,l(n3,l(n4,l(end,l(n5,l(n6,l(n7,l(n8,l(end,l(n9,l(n10,l(n11,l(n12,l(end,l(n13,l(n15,l(n14,l(x,l(end,nil))))))))))))))))))))) )).

cnf(final_state,negated_conjecture,
    ( ~ state(l(n1,l(n2,l(n3,l(n4,l(end,l(n5,l(n6,l(n7,l(n8,l(end,l(n9,l(n10,l(n11,l(n12,l(end,l(n13,l(n14,l(n15,l(x,l(end,nil))))))))))))))))))))) )).

cnf(shift_sideways,axiom,
    ( l(x,l(A,R)) = l(A,l(x,R))
    | A = end )).

cnf(shift_up_down,axiom,
    ( l(x,l(A,l(B,l(C,l(D,l(E,R)))))) = l(E,l(A,l(B,l(C,l(D,l(x,R)))))) )).

%--------------------------------------------------------------------------
