%--------------------------------------------------------------------------
% File     : NLP121-1 : TPTP v6.2.0. Released v2.4.0.
% Domain   : Natural Language Processing
% Problem  : An old dirty white Chevy, problem 8
% Version  : [Bos00b] axioms.
% English  : Eliminating logically equivalent interpretations in the statement
%            "An old dirty white chevy barrels down a lonely street in
%            hollywood."

% Refs     : [Bos00a] Bos (2000), DORIS: Discourse Oriented Representation a
%            [Bos00b] Bos (2000), Applied Theorem Proving - Natural Language
% Source   : [TPTP]
% Names    :

% Status   : Satisfiable
% Rating   : 0.00 v2.4.0
% Syntax   : Number of clauses     :   36 (  16 non-Horn;   2 unit;  36 RR)
%            Number of atoms       :  102 (   0 equality)
%            Maximal clause size   :   18 (   3 average)
%            Number of predicates  :   18 (   1 propositional; 0-3 arity)
%            Number of functors    :   10 (  10 constant; 0-0 arity)
%            Number of variables   :   10 (   0 singleton)
%            Maximal term depth    :    1 (   1 average)
% SPC      : CNF_SAT_EPR

% Comments : Created from NLP121+1.p using FLOTTER
%--------------------------------------------------------------------------
cnf(clause1,negated_conjecture,
    ( actual_world(skc15) )).

cnf(clause2,negated_conjecture,
    ( actual_world(skc10) )).

cnf(clause3,negated_conjecture,
    ( ssSkC0
    | city(skc15,skc18) )).

cnf(clause4,negated_conjecture,
    ( ssSkC0
    | chevy(skc15,skc18) )).

cnf(clause5,negated_conjecture,
    ( ssSkC0
    | white(skc15,skc18) )).

cnf(clause6,negated_conjecture,
    ( ssSkC0
    | dirty(skc15,skc18) )).

cnf(clause7,negated_conjecture,
    ( ssSkC0
    | old(skc15,skc18) )).

cnf(clause8,negated_conjecture,
    ( ssSkC0
    | placename(skc15,skc19) )).

cnf(clause9,negated_conjecture,
    ( ssSkC0
    | hollywood_placename(skc15,skc19) )).

cnf(clause10,negated_conjecture,
    ( ssSkC0
    | event(skc15,skc16) )).

cnf(clause11,negated_conjecture,
    ( ssSkC0
    | present(skc15,skc16) )).

cnf(clause12,negated_conjecture,
    ( ssSkC0
    | barrel(skc15,skc16) )).

cnf(clause13,negated_conjecture,
    ( ssSkC0
    | lonely(skc15,skc17) )).

cnf(clause14,negated_conjecture,
    ( ssSkC0
    | street(skc15,skc17) )).

cnf(clause15,negated_conjecture,
    ( ~ ssSkC0
    | city(skc10,skc13) )).

cnf(clause16,negated_conjecture,
    ( ~ ssSkC0
    | street(skc10,skc13) )).

cnf(clause17,negated_conjecture,
    ( ~ ssSkC0
    | lonely(skc10,skc13) )).

cnf(clause18,negated_conjecture,
    ( ~ ssSkC0
    | placename(skc10,skc14) )).

cnf(clause19,negated_conjecture,
    ( ~ ssSkC0
    | hollywood_placename(skc10,skc14) )).

cnf(clause20,negated_conjecture,
    ( ~ ssSkC0
    | event(skc10,skc11) )).

cnf(clause21,negated_conjecture,
    ( ~ ssSkC0
    | present(skc10,skc11) )).

cnf(clause22,negated_conjecture,
    ( ~ ssSkC0
    | barrel(skc10,skc11) )).

cnf(clause23,negated_conjecture,
    ( ~ ssSkC0
    | old(skc10,skc12) )).

cnf(clause24,negated_conjecture,
    ( ~ ssSkC0
    | dirty(skc10,skc12) )).

cnf(clause25,negated_conjecture,
    ( ~ ssSkC0
    | white(skc10,skc12) )).

cnf(clause26,negated_conjecture,
    ( ~ ssSkC0
    | chevy(skc10,skc12) )).

cnf(clause27,negated_conjecture,
    ( ssSkC0
    | agent(skc15,skc16,skc18) )).

cnf(clause28,negated_conjecture,
    ( ssSkC0
    | in(skc15,skc16,skc18) )).

cnf(clause29,negated_conjecture,
    ( ssSkC0
    | of(skc15,skc19,skc18) )).

cnf(clause30,negated_conjecture,
    ( ssSkC0
    | down(skc15,skc16,skc17) )).

cnf(clause31,negated_conjecture,
    ( ~ ssSkC0
    | down(skc10,skc11,skc13) )).

cnf(clause32,negated_conjecture,
    ( ~ ssSkC0
    | in(skc10,skc11,skc13) )).

cnf(clause33,negated_conjecture,
    ( ~ ssSkC0
    | of(skc10,skc14,skc13) )).

cnf(clause34,negated_conjecture,
    ( ~ ssSkC0
    | agent(skc10,skc11,skc12) )).

cnf(clause35,negated_conjecture,
    ( ~ city(U,V)
    | ~ street(U,V)
    | ~ lonely(U,V)
    | ~ down(U,W,V)
    | ~ in(U,W,V)
    | ~ placename(U,X)
    | ~ hollywood_placename(U,X)
    | ~ of(U,X,V)
    | ~ event(U,W)
    | ~ present(U,W)
    | ~ barrel(U,W)
    | ~ agent(U,W,Y)
    | ~ old(U,Y)
    | ~ dirty(U,Y)
    | ~ white(U,Y)
    | ~ chevy(U,Y)
    | ~ actual_world(U)
    | ssSkC0 )).

cnf(clause36,negated_conjecture,
    ( ~ city(U,V)
    | ~ chevy(U,V)
    | ~ white(U,V)
    | ~ dirty(U,V)
    | ~ old(U,V)
    | ~ agent(U,W,V)
    | ~ in(U,W,V)
    | ~ placename(U,X)
    | ~ hollywood_placename(U,X)
    | ~ of(U,X,V)
    | ~ event(U,W)
    | ~ present(U,W)
    | ~ barrel(U,W)
    | ~ down(U,W,Y)
    | ~ lonely(U,Y)
    | ~ street(U,Y)
    | ~ actual_world(U)
    | ~ ssSkC0 )).

%--------------------------------------------------------------------------
