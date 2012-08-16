%--------------------------------------------------------------------------
% File     : PUZ024-1 : TPTP v5.2.0. Released v1.0.0.
% Domain   : Puzzles
% Problem  : Knights and Knaves #31
% Version  : Especial.
% English  : There is an island with exactly two types of people -
%            truthtellers who always tell the truth and liars who always lie.
%            There are a group of three people, A, B, and C on the island. A
%            and B make the following statements. A: All of us are liars;
%            B: Exactly one of us is a truthteller. What are A, B, and C?
%            Answer: A is a liar, B is a truthteller, and C is a liar.

% Refs     : [Smu78] Smullyan (1978), What is the Name of This Book? The Ri
% Source   : [ANL]
% Names    : Problem 31 [Smu78]
%          : tandl31.ver1.in [ANL]

% Status   : Unsatisfiable
% Rating   : 0.00 v2.0.0
% Syntax   : Number of clauses     :   20 (   6 non-Horn;   6 unit;  19 RR)
%            Number of atoms       :   60 (   0 equality)
%            Maximal clause size   :    5 (   3 average)
%            Number of predicates  :    2 (   0 propositional; 1-3 arity)
%            Number of functors    :    8 (   5 constant; 0-2 arity)
%            Number of variables   :   35 (   3 singleton)
%            Maximal term depth    :    2 (   1 average)
% SPC      : CNF_UNS_RFO_NEQ_NHN

% Comments :
%--------------------------------------------------------------------------
%----Include axioms for truthtellers and liars
include('Axioms/PUZ002-0.ax').
%--------------------------------------------------------------------------
%----The next 6 clauses define what is meant by exactly one
%----is a truthteller.
cnf(one_says_one_truthteller,axiom,
    ( ~ a_truth(says(X,one_truthteller))
    | ~ people(X,Y,Z)
    | ~ a_truth(truthteller(X))
    | ~ a_truth(truthteller(Y))
    | ~ a_truth(truthteller(Z)) )).

cnf(one_is_the_truth_teller,axiom,
    ( ~ a_truth(one_truthteller)
    | ~ people(X,Y,Z)
    | a_truth(truthteller(X))
    | a_truth(truthteller(Y))
    | a_truth(truthteller(Z)) )).

cnf(one_is_not_the_truthteller,axiom,
    ( ~ a_truth(one_truthteller)
    | ~ people(X,Y,Z)
    | ~ a_truth(truthteller(Y))
    | ~ a_truth(truthteller(X))
    | ~ a_truth(truthteller(Z)) )).

cnf(one_truthteller_two_liars,axiom,
    ( ~ a_truth(one_truthteller)
    | ~ people(X,Y,Z)
    | ~ a_truth(liar(X))
    | ~ a_truth(truthteller(Y))
    | a_truth(liar(Z)) )).

cnf(two_truthtellers,axiom,
    ( a_truth(one_truthteller)
    | ~ people(X,Y,Z)
    | ~ a_truth(liar(X))
    | ~ a_truth(truthteller(Y))
    | a_truth(truthteller(Z)) )).

cnf(three_liars,axiom,
    ( a_truth(one_truthteller)
    | ~ people(X,Y,Z)
    | ~ a_truth(liar(X))
    | ~ a_truth(liar(Y))
    | a_truth(liar(Z)) )).

%----If x says that 'all of us' are liars then, all the persons may
%----be liars.
cnf(speaker_is_lying,axiom,
    ( ~ a_truth(says(X,all_are_liars))
    | ~ people(X,Y,Z)
    | ~ a_truth(truthteller(X)) )).

cnf(all_truthtellers,axiom,
    ( a_truth(all_are_liars)
    | ~ people(X,Y,Z)
    | a_truth(truthteller(X))
    | a_truth(truthteller(Y))
    | a_truth(truthteller(Z)) )).

cnf(b_c_a_people,hypothesis,
    ( people(b,c,a) )).

cnf(a_c_b_people,hypothesis,
    ( people(a,c,b) )).

cnf(c_b_a_people,hypothesis,
    ( people(c,b,a) )).

cnf(a_says_all_are_liars,hypothesis,
    ( a_truth(says(a,all_are_liars)) )).

cnf(b_says_one_truthteller,hypothesis,
    ( a_truth(says(b,one_truthteller)) )).

%----This is an honest way of doing this. A simpler version could simply
%----prove that A and C are a liars, and  B is a truthteller
cnf(prove_there_is_a_truthteller,negated_conjecture,
    ( ~ a_truth(truthteller(Truthteller)) )).

%--------------------------------------------------------------------------
