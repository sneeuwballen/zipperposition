%--------------------------------------------------------------------------
% File     : PUZ028-5 : TPTP v5.2.0. Released v2.0.0.
% Domain   : Puzzles
% Problem  : People at a party
% Version  : [ICO92] axioms : Especial.
% English  : We can always choose 3 persons who are either familiar with
%            each other or not familiar with each other, from 6 persons
%            who meet at a party.

% Refs     : [ICO92] ICOT (1992), Model Generation Theorem Prover, MGTP
%          : [Gei95] Geisler (1995), Email to C. Suttner
% Source   : [TPTP]
% Names    :

% Status   : Unsatisfiable
% Rating   : 0.00 v2.4.0, 0.25 v2.3.0, 0.00 v2.2.0, 0.67 v2.1.0
% Syntax   : Number of clauses     :   15 (   1 non-Horn;  11 unit;  15 RR)
%            Number of atoms       :   25 (   0 equality)
%            Maximal clause size   :    5 (   2 average)
%            Number of predicates  :    4 (   0 propositional; 1-2 arity)
%            Number of functors    :    6 (   6 constant; 0-0 arity)
%            Number of variables   :   11 (   0 singleton)
%            Maximal term depth    :    1 (   1 average)
% SPC      : CNF_UNS_EPR

% Comments : The version in [ICO92] gets the order of arguments in
%            the theorem clauses wrong, and is satisfiable as a result.
%            The intention was clearly to proof the statement given above
%            in the % English field. This version corrects it. The TUM
%            versions of this problem suffer the same fate as the ICOT
%            versions. Tim Geisler [Gei95] pointed this out.
%--------------------------------------------------------------------------
cnf(person1,axiom,
    ( person(one) )).

cnf(person2,axiom,
    ( person(two) )).

cnf(person3,axiom,
    ( person(three) )).

cnf(person4,axiom,
    ( person(four) )).

cnf(person5,axiom,
    ( person(five) )).

cnf(person6,axiom,
    ( person(six) )).

cnf(order1,axiom,
    ( after(one,two) )).

cnf(order2,axiom,
    ( after(two,three) )).

cnf(order3,axiom,
    ( after(three,four) )).

cnf(order4,axiom,
    ( after(four,five) )).

cnf(order5,axiom,
    ( after(five,six) )).

cnf(transitivity_of_order,axiom,
    ( after(Large,Small)
    | ~ after(Large,Medium)
    | ~ after(Medium,Small) )).

cnf(familiar_or_not,axiom,
    ( familiar(X,Y)
    | not_familiar(X,Y)
    | ~ person(X)
    | ~ person(Y)
    | ~ after(X,Y) )).

cnf(three_familiar,negated_conjecture,
    ( ~ familiar(X1,X2)
    | ~ familiar(X2,X3)
    | ~ familiar(X1,X3) )).

cnf(three_not_familiar,negated_conjecture,
    ( ~ not_familiar(X1,X2)
    | ~ not_familiar(X2,X3)
    | ~ not_familiar(X1,X3) )).

%--------------------------------------------------------------------------
