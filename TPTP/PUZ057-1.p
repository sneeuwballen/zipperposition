%--------------------------------------------------------------------------
% File     : PUZ057-1 : TPTP v5.2.0. Released v2.7.0.
% Domain   : Puzzles
% Problem  : Show the Hanoi problem is not solvable anymore
% Version  : [Cla03] axioms : Especial.
% English  : Move blocks from pin to pin, until a goal configuration
%            is reached. Some rules are removed: one can only move a block
%            to a pin next to it; one cannot place the small block on
%            the large block.

% Refs     : [Cla03] Claessen (2003), Email to G. Sutcliffe
% Source   : [Cla03]
% Names    :

% Status   : Satisfiable
% Rating   : 1.00 v3.2.0, 0.80 v3.1.0, 0.67 v2.7.0
% Syntax   : Number of clauses     :    9 (   0 non-Horn;   8 unit;   3 RR)
%            Number of atoms       :   11 (   2 equality)
%            Maximal clause size   :    3 (   1 average)
%            Number of predicates  :    3 (   0 propositional; 1-2 arity)
%            Number of functors    :    9 (   6 constant; 0-2 arity)
%            Number of variables   :   11 (   5 singleton)
%            Maximal term depth    :    7 (   2 average)
% SPC      : CNF_SAT_RFO_EQU_NUE

% Comments : Alternative axiomatization, suited for model finding
%--------------------------------------------------------------------------
cnf(initial_state,axiom,
    ( state(nextto(on(small,on(medium,on(large,pin(n1)))),nextto(pin(n2),pin(n3)))) )).

cnf(final_state,negated_conjecture,
    ( ~ state(nextto(pin(n1),nextto(pin(n2),on(small,on(medium,on(large,pin(n3))))))) )).

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

%input_clause(fits_6,axiom,
%   [++fits(small,on(large,A))]).
%--------------------------------------------------------------------------
