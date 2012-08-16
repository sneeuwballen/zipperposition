%--------------------------------------------------------------------------
% File     : PUZ027-1 : TPTP v5.2.0. Released v1.0.0.
% Domain   : Puzzles
% Problem  : Knights and Knaves #42
% Version  : Especial.
% English  : There  is an island with exactly three types of people -
%            truthtellers who always tell the truth, and liars who always
%            lie, and normals who sometimes tell the truth and sometimes
%            lie. Liars are said to be of the lowest rank, normals are
%            middle rank, and truthtellers of the highest rank. Two people
%            A and B on the island make the following statements. A: I am
%            of lower rank than B. B: That's not true! What are the ranks
%            of A and B, and which of the two statements are true? Answer:
%            A is a normal and B is a normal.

% Refs     : [Smu78] Smullyan (1978), What is the Name of This Book? The Ri
% Source   : [ANL]
% Names    : Problem 42 [Smu78]
%          : tandl42.ver1.in [ANL]

% Status   : Unsatisfiable
% Rating   : 0.00 v2.0.0
% Syntax   : Number of clauses     :   32 (   9 non-Horn;   4 unit;  29 RR)
%            Number of atoms       :   88 (   0 equality)
%            Maximal clause size   :    4 (   3 average)
%            Number of predicates  :    2 (   0 propositional; 1-1 arity)
%            Number of functors    :   17 (  11 constant; 0-2 arity)
%            Number of variables   :   36 (   3 singleton)
%            Maximal term depth    :    3 (   2 average)
% SPC      : CNF_UNS_RFO_NEQ_NHN

% Comments :
%--------------------------------------------------------------------------
%----Include axioms on truthtellers, liars and normal people
include('Axioms/PUZ003-0.ax').
%--------------------------------------------------------------------------
%----The next 12 clause define lower thand and not lower than
cnf(not_lower_is_irreflexive,axiom,
    ( a_truth(not_lower(X,X)) )).

cnf(not_not_lower_and_lower,axiom,
    ( ~ a_truth(not_lower(X,Y))
    | ~ a_truth(lower(X,Y)) )).

cnf(not_lower_or_lower,axiom,
    ( a_truth(not_lower(X,Y))
    | a_truth(lower(X,Y)) )).

cnf(liars_lowest,axiom,
    ( ~ a_truth(lower(X,Y))
    | ~ a_truth(liar(X))
    | a_truth(normal(Y))
    | a_truth(truthteller(Y)) )).

cnf(truthtellers_highest,axiom,
    ( ~ a_truth(lower(X,Y))
    | ~ a_truth(normal(X))
    | a_truth(truthteller(Y)) )).

cnf(truthtellers_lower_than_no_one,axiom,
    ( ~ a_truth(lower(X,Y))
    | ~ a_truth(truthteller(X)) )).

cnf(normal_and_liars_lower_than_truthtellers,axiom,
    ( ~ a_truth(lower(X,Y))
    | ~ a_truth(truthteller(Y))
    | a_truth(normal(X))
    | a_truth(liar(X)) )).

cnf(liars_lower_than_normal,axiom,
    ( ~ a_truth(lower(X,Y))
    | ~ a_truth(normal(Y))
    | a_truth(liar(X)) )).

cnf(no_one_lower_than_liars,axiom,
    ( ~ a_truth(lower(X,Y))
    | ~ a_truth(liar(Y)) )).

cnf(not_lower_than_truthteller,axiom,
    ( ~ a_truth(not_lower(X,Y))
    | ~ a_truth(truthteller(X))
    | a_truth(truthteller(Y))
    | a_truth(lower(Y,X)) )).

cnf(not_lower_than_liar,axiom,
    ( ~ a_truth(not_lower(X,Y))
    | ~ a_truth(liar(X))
    | a_truth(liar(Y))
    | a_truth(lower(Y,X)) )).

cnf(not_lower_than_normal,axiom,
    ( ~ a_truth(not_lower(X,Y))
    | ~ a_truth(normal(X))
    | a_truth(normal(Y))
    | a_truth(lower(Y,X)) )).

cnf(a_says_a_lower_than_b,hypothesis,
    ( a_truth(says(a,lower(a,b))) )).

cnf(b_says_a_not_lower_than_b,hypothesis,
    ( a_truth(says(b,not_lower(a,b))) )).

%----This is an honest way of doing this. A simpler version could simply
%----prove that A is a normal and B is a normal.
cnf(a_and_b_truthteller,hypothesis,
    ( ~ a_truth(truthteller(a))
    | ~ a_truth(truthteller(b))
    | answer(a_and_b_truthteller) )).

cnf(a_truthteller_b_normal,hypothesis,
    ( ~ a_truth(truthteller(a))
    | ~ a_truth(normal(b))
    | answer(a_truthteller_b_normal) )).

cnf(a_truthteller_b_liar,hypothesis,
    ( ~ a_truth(truthteller(a))
    | ~ a_truth(liar(b))
    | answer(a_truthteller_b_liar) )).

cnf(a_normal_b_truthteller,hypothesis,
    ( ~ a_truth(normal(a))
    | ~ a_truth(truthteller(b))
    | answer(a_normal_b_truthteller) )).

cnf(a_and_b_normal,hypothesis,
    ( ~ a_truth(normal(a))
    | ~ a_truth(normal(b))
    | answer(a_and_b_normal) )).

cnf(a_normal_b_liar,hypothesis,
    ( ~ a_truth(normal(a))
    | ~ a_truth(liar(b))
    | answer(a_normal_b_liar) )).

cnf(a_liar_b_truthteller,hypothesis,
    ( ~ a_truth(liar(a))
    | ~ a_truth(truthteller(b))
    | answer(a_liar_b_truthteller) )).

cnf(a_liar_b_normal,hypothesis,
    ( ~ a_truth(liar(a))
    | ~ a_truth(normal(b))
    | answer(a_liar_b_normal) )).

cnf(a_and_b_liar,hypothesis,
    ( ~ a_truth(liar(a))
    | ~ a_truth(liar(b))
    | answer(a_and_b_liar) )).

cnf(prove_there_is_an_answer,negated_conjecture,
    ( ~ answer(Answer) )).

%--------------------------------------------------------------------------
