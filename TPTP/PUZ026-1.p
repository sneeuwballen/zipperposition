%--------------------------------------------------------------------------
% File     : PUZ026-1 : TPTP v5.2.0. Released v1.0.0.
% Domain   : Puzzles
% Problem  : Knights and Knaves #39
% Version  : Especial.
% English  : There is an island with exactly three types of people -
%            truthtellers who always tell the truth, and liars who always
%            lie, and normals who sometimes tell the truth and sometimes
%            lie. We are given three people, A, B, C, one of whom is a
%            truthteller, one a liar, and one a normal (but not neccesarily
%            in that order). They make the following statements. A: I am
%            normal; B: That is true. C: I am not normal. What are A,B,
%            and C? Answer: A is a liar, B is a normal, and C is a truthteller.

% Refs     : [Smu78] Smullyan (1978), What is the Name of This Book? The Ri
% Source   : [ANL]
% Names    : Problem 39 [Smu78]
%          : tandl39.ver1.in [ANL]

% Status   : Unsatisfiable
% Rating   : 0.00 v2.5.0, 0.20 v2.4.0, 0.00 v2.1.0, 0.00 v2.0.0
% Syntax   : Number of clauses     :   23 (   4 non-Horn;   6 unit;  21 RR)
%            Number of atoms       :   54 (   0 equality)
%            Maximal clause size   :    4 (   2 average)
%            Number of predicates  :    2 (   0 propositional; 1-3 arity)
%            Number of functors    :    8 (   3 constant; 0-2 arity)
%            Number of variables   :   35 (   9 singleton)
%            Maximal term depth    :    3 (   2 average)
% SPC      : CNF_UNS_RFO_NEQ_NHN

% Comments :
%--------------------------------------------------------------------------
%----Include axioms on truthtellers, liars and normal people
include('Axioms/PUZ003-0.ax').
%--------------------------------------------------------------------------
cnf(not_normal_and_not_normal,axiom,
    ( ~ a_truth(not_normal(X))
    | ~ a_truth(normal(X)) )).

cnf(normal_or_not_normal,axiom,
    ( a_truth(not_normal(X))
    | a_truth(normal(X)) )).

%----The next 6 clause says there is one each of normal, truthteller and
%----liar.
cnf(not_two_truthtellers1,axiom,
    ( ~ people(X,Y,Z)
    | ~ a_truth(truthteller(X))
    | ~ a_truth(truthteller(Y)) )).

cnf(not_two_truthtellers2,axiom,
    ( ~ people(X,Y,Z)
    | ~ a_truth(truthteller(X))
    | ~ a_truth(truthteller(Z)) )).

cnf(not_two_liars1,axiom,
    ( ~ people(X,Y,Z)
    | ~ a_truth(liar(X))
    | ~ a_truth(liar(Y)) )).

cnf(not_two_liars2,axiom,
    ( ~ people(X,Y,Z)
    | ~ a_truth(liar(X))
    | ~ a_truth(liar(Z)) )).

cnf(not_two_normal1,axiom,
    ( ~ people(X,Y,Z)
    | ~ a_truth(normal(X))
    | ~ a_truth(normal(Y)) )).

cnf(not_two_normal2,axiom,
    ( ~ people(X,Y,Z)
    | ~ a_truth(normal(X))
    | ~ a_truth(normal(Z)) )).

cnf(a_b_c_are_people,hypothesis,
    ( people(a,b,c) )).

cnf(b_c_a_are_people,hypothesis,
    ( people(b,c,a) )).

cnf(c_b_a_are_people,hypothesis,
    ( people(c,b,a) )).

cnf(a_says_a_normal,hypothesis,
    ( a_truth(says(a,normal(a))) )).

cnf(b_says_a_normal,hypothesis,
    ( a_truth(says(b,normal(a))) )).

cnf(c_says_c_not_normal,hypothesis,
    ( a_truth(says(c,not_normal(c))) )).

cnf(prove_one_of_each,negated_conjecture,
    ( ~ a_truth(liar(Liar))
    | ~ a_truth(normal(Normal))
    | ~ a_truth(truthteller(Truthteller)) )).

%--------------------------------------------------------------------------
