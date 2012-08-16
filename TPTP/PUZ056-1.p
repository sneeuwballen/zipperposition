%--------------------------------------------------------------------------
% File     : PUZ056-1 : TPTP v5.2.0. Released v2.7.0.
% Domain   : Puzzles
% Problem  : Tower of Hanoi
% Version  : [Cla03] axioms : Especial.
% English  :

% Refs     : [Cla03] Claessen (2003), Email to G. Sutcliffe
% Source   : [Cla03]
% Names    :

% Status   : Unsatisfiable
% Rating   : 0.67 v5.2.0, 0.38 v5.1.0, 0.43 v4.1.0, 0.44 v4.0.1, 0.17 v3.3.0, 0.14 v3.1.0, 0.56 v2.7.0
% Syntax   : Number of clauses     :   11 (   0 non-Horn;  10 unit;   3 RR)
%            Number of atoms       :   13 (   3 equality)
%            Maximal clause size   :    3 (   1 average)
%            Number of predicates  :    3 (   0 propositional; 1-2 arity)
%            Number of functors    :    9 (   6 constant; 0-2 arity)
%            Number of variables   :   14 (   6 singleton)
%            Maximal term depth    :    7 (   2 average)
% SPC      : CNF_UNS_RFO_SEQ_HRN

% Comments : Alternative axiomatization, suited for model finding
%--------------------------------------------------------------------------
cnf(initial_state,axiom,
    ( state(nextto(on(small,on(medium,on(large,pin(n1)))),nextto(pin(n2),pin(n3)))) )).

cnf(final_state,negated_conjecture,
    ( ~ state(nextto(pin(n1),nextto(pin(n2),on(small,on(medium,on(large,pin(n3))))))) )).

cnf(nextto_commutative,axiom,
    ( nextto(X,Y) = nextto(Y,X) )).

cnf(nextto_associative,axiom,
    ( nextto(X,nextto(Y,Z)) = nextto(nextto(X,Y),Z) )).

cnf(a_move,axiom,
    ( nextto(on(X,A),B) = nextto(A,on(X,B))
    | ~ fits(X,B)
    | ~ fits(X,A) )).

cnf(fits_1,axiom,
    ( fits(small,pin(N)) )).

cnf(fits_2,axiom,
    ( fits(medium,pin(N)) )).

cnf(fits_3,axiom,
    ( fits(large,pin(N)) )).

cnf(fits_4,axiom,
    ( fits(small,on(medium,A)) )).

cnf(fits_5,axiom,
    ( fits(medium,on(large,A)) )).

cnf(fits_6,axiom,
    ( fits(small,on(large,A)) )).

%--------------------------------------------------------------------------
