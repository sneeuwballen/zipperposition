%--------------------------------------------------------------------------
% File     : PUZ025-1 : TPTP v5.2.0. Released v1.0.0.
% Domain   : Puzzles
% Problem  : Knights and Knaves #35
% Version  : Especial.
% English  : There is an island with exactly two types of people -
%            truthtellers who always tell the truth and liars who always lie.
%            There are a group of three people, A, B, and C on the island.
%            A and B make the following statements. A: "B and C are the same
%            type". Someone asks "Are A and B of the same type ? " What does
%            C answer? Answer: "yes"

% Refs     : [Smu78] Smullyan (1978), What is the Name of This Book? The Ri
% Source   : [ANL]
% Names    : Problem 35 [Smu78]
%          : tandl35.ver1.in [ANL]

% Status   : Unsatisfiable
% Rating   : 0.00 v2.0.0
% Syntax   : Number of clauses     :   24 (   6 non-Horn;   7 unit;  21 RR)
%            Number of atoms       :   61 (   0 equality)
%            Maximal clause size   :    4 (   3 average)
%            Number of predicates  :    4 (   0 propositional; 1-3 arity)
%            Number of functors    :    9 (   5 constant; 0-2 arity)
%            Number of variables   :   35 (   3 singleton)
%            Maximal term depth    :    3 (   2 average)
% SPC      : CNF_UNS_RFO_NEQ_NHN

% Comments :
%--------------------------------------------------------------------------
%----Include axioms for truthtellers and liars
include('Axioms/PUZ002-0.ax').
%--------------------------------------------------------------------------
%----The next 6 clause define what is meant by being of equal type
cnf(two_liars_are_equal,axiom,
    ( ~ people(X,Y,Z)
    | ~ a_truth(liar(X))
    | ~ a_truth(liar(Y))
    | a_truth(equal_type(X,Y)) )).

cnf(two_truthtellers_are_equal,axiom,
    ( ~ people(X,Y,Z)
    | ~ a_truth(truthteller(X))
    | ~ a_truth(truthteller(Y))
    | a_truth(equal_type(X,Y)) )).

cnf(truthteller_equals_truthtller,axiom,
    ( ~ a_truth(equal_type(X,Y))
    | ~ a_truth(truthteller(X))
    | a_truth(truthteller(Y)) )).

cnf(liar_equals_liar,axiom,
    ( ~ a_truth(equal_type(X,Y))
    | ~ a_truth(liar(X))
    | a_truth(liar(Y)) )).

cnf(truthteller_not_equal_liar,axiom,
    ( ~ a_truth(truthteller(X))
    | a_truth(equal_type(X,Y))
    | a_truth(liar(Y)) )).

cnf(liar_not_equal_truthteller,axiom,
    ( ~ a_truth(liar(X))
    | a_truth(equal_type(X,Y))
    | a_truth(truthteller(Y)) )).

cnf(symmetry_of_equal_type,axiom,
    ( ~ a_truth(equal_type(X,Y))
    | a_truth(equal_type(Y,X)) )).

cnf(truthteller_identifies_truths,axiom,
    ( ~ ask_1_if_2(X,Y)
    | ~ a_truth(truthteller(X))
    | ~ a_truth(Y)
    | answer(yes) )).

cnf(truthteller_denies_lies,axiom,
    ( ~ ask_1_if_2(X,Y)
    | ~ a_truth(truthteller(X))
    | a_truth(Y)
    | answer(no) )).

cnf(liar_denies_truths,axiom,
    ( ~ ask_1_if_2(X,Y)
    | ~ a_truth(liar(X))
    | ~ a_truth(Y)
    | answer(no) )).

cnf(liar_supports_lies,axiom,
    ( ~ ask_1_if_2(X,Y)
    | ~ a_truth(liar(X))
    | a_truth(Y)
    | answer(yes) )).

cnf(b_c_a_are_people,hypothesis,
    ( people(b,c,a) )).

%----bugs here
cnf(a_b_c_are_people,hypothesis,
    ( people(a,b,a) )).

cnf(a_c_b_are_people,hypothesis,
    ( people(a,c,b) )).

cnf(c_b_a_are_people,hypothesis,
    ( people(c,b,a) )).

cnf(a_says_b_and_c_equal,hypothesis,
    ( a_truth(says(a,equal_type(b,c))) )).

cnf(ask_c_if_a_and_b_equal,hypothesis,
    ( ask_1_if_2(c,equal_type(a,b)) )).

cnf(prove_there_is_an_answer,negated_conjecture,
    ( ~ answer(Answer) )).

%--------------------------------------------------------------------------
