%--------------------------------------------------------------------------
% File     : PUZ023-1 : TPTP v5.2.0. Released v1.0.0.
% Domain   : Puzzles
% Problem  : Knights and Knaves #27
% Version  : Especial.
% English  : There is an island with exactly two types of people :
%            truthtellers who always tell the truth and liars who always
%            lie. There are a group of three people, A, B, and C on the
%            island. A stranger passes by and asks A, "How many
%            truthtellers are among you ?" A answers indistinctly. So the
%            stranger asks B, "what did A say?". B replies "A said that
%            there is exactly one truthteller among us." Then C says,
%            "Don't believe B; he is lying!" What are B and C. Answer:
%            B is a liar and C is a truth-teller.

% Refs     : [Smu78] Smullyan (1978), What is the Name of This Book? The Ri
% Source   : [ANL]
% Names    : Problem 27 [Smu78]
%          : tandl27.ver1.in [ANL]

% Status   : Unsatisfiable
% Rating   : 0.00 v2.0.0
% Syntax   : Number of clauses     :   22 (   6 non-Horn;   5 unit;  21 RR)
%            Number of atoms       :   65 (   0 equality)
%            Maximal clause size   :    5 (   3 average)
%            Number of predicates  :    3 (   0 propositional; 1-3 arity)
%            Number of functors    :   12 (   9 constant; 0-2 arity)
%            Number of variables   :   32 (   4 singleton)
%            Maximal term depth    :    3 (   2 average)
% SPC      : CNF_UNS_RFO_NEQ_NHN

% Comments :
%--------------------------------------------------------------------------
%----Include axioms for truthtellers and liars
include('Axioms/PUZ002-0.ax').
%--------------------------------------------------------------------------
cnf(one_is_the_truthteller,axiom,
    ( ~ people(X,Y,Z)
    | ~ a_truth(one_truthteller)
    | a_truth(truthteller(X))
    | a_truth(truthteller(Y))
    | a_truth(truthteller(Z)) )).

cnf(two_truthtellers1,axiom,
    ( ~ people(X,Y,Z)
    | ~ a_truth(truthteller(X))
    | ~ a_truth(truthteller(Y))
    | ~ a_truth(one_truthteller) )).

cnf(two_truthtellers2,axiom,
    ( ~ people(X,Y,Z)
    | ~ a_truth(truthteller(X))
    | ~ a_truth(truthteller(Z))
    | ~ a_truth(one_truthteller) )).

cnf(two_truthtellers3,axiom,
    ( ~ people(X,Y,Z)
    | ~ a_truth(truthteller(Y))
    | ~ a_truth(truthteller(Z))
    | ~ a_truth(one_truthteller) )).

cnf(identify_one_truthteller1,axiom,
    ( ~ people(X,Y,Z)
    | a_truth(one_truthteller)
    | ~ a_truth(truthteller(X))
    | a_truth(truthteller(Y))
    | a_truth(truthteller(Z)) )).

cnf(identify_one_truthteller2,axiom,
    ( ~ people(X,Y,Z)
    | a_truth(one_truthteller)
    | ~ a_truth(truthteller(Y))
    | a_truth(truthteller(X))
    | a_truth(truthteller(Z)) )).

cnf(identify_one_truthteller3,axiom,
    ( ~ people(X,Y,Z)
    | a_truth(one_truthteller)
    | ~ a_truth(truthteller(Z))
    | a_truth(truthteller(Y))
    | a_truth(truthteller(X)) )).

cnf(a_b_and_c_are_people,hypothesis,
    ( people(a,b,c) )).

cnf(a_says_garbage,hypothesis,
    ( a_truth(says(a,garbage)) )).

cnf(b_says_a_says_one_truthteller,hypothesis,
    ( a_truth(says(b,says(a,one_truthteller))) )).

cnf(c_says_b_lies,hypothesis,
    ( a_truth(says(c,liar(b))) )).

%----This is an honest way of doing this. A simpler version could simply
%----prove that B is a liar and C is a truth-teller.
cnf(b_and_c_liars,hypothesis,
    ( ~ a_truth(liar(b))
    | ~ a_truth(liar(c))
    | an_answer(b_and_c_liars) )).

cnf(b_liar_and_c_truthteller,hypothesis,
    ( ~ a_truth(liar(b))
    | ~ a_truth(truthteller(c))
    | an_answer(b_liar_and_c_truthteller) )).

cnf(b_truthteller_and_c_liar,hypothesis,
    ( ~ a_truth(truthteller(b))
    | ~ a_truth(liar(c))
    | an_answer(b_truthteller_and_c_liar) )).

cnf(b_and_c_truthtellers,hypothesis,
    ( ~ a_truth(truthteller(b))
    | ~ a_truth(truthteller(c))
    | an_answer(b_and_c_truthtellers) )).

cnf(prove_there_is_an_answer,negated_conjecture,
    ( ~ an_answer(X) )).

%--------------------------------------------------------------------------
