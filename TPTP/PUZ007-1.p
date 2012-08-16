%--------------------------------------------------------------------------
% File     : PUZ007-1 : TPTP v5.2.0. Released v1.0.0.
% Domain   : Puzzles
% Problem  : Mixed couples on Mars and Venus
% Version  : Especial.
% English  : Here's the situation: human observers in this exclusive club
%            on Ganymede can't distinguish Martians from Venusians, males
%            from females, except for the fact that Venusian women and
%            Martian men always tell the truth and Venusian men and
%            Martian women always lie.
%            A says "I'm from Mars" B exclaims "That's not true!" A and
%            B are married; are they a mixed couple?

% Refs     :
% Source   : [ANL]
% Names    : mars_venus2.in [ANL]

% Status   : Unsatisfiable
% Rating   : 0.11 v5.2.0, 0.12 v5.1.0, 0.06 v5.0.0, 0.00 v3.3.0, 0.07 v3.2.0, 0.00 v2.0.0
% Syntax   : Number of clauses     :   28 (   7 non-Horn;   4 unit;  24 RR)
%            Number of atoms       :   59 (   3 equality)
%            Maximal clause size   :    3 (   2 average)
%            Number of predicates  :    9 (   0 propositional; 1-2 arity)
%            Number of functors    :    5 (   4 constant; 0-1 arity)
%            Number of variables   :   20 (   1 singleton)
%            Maximal term depth    :    2 (   1 average)
% SPC      : CNF_UNS_RFO_SEQ_NHN

% Comments :
%--------------------------------------------------------------------------
%----Include Mars and Venus axioms
include('Axioms/PUZ001-0.ax').
%--------------------------------------------------------------------------
cnf(a_says_hes_from_mars,hypothesis,
    ( says(a,a_from_mars) )).

cnf(b_says_a_lies,hypothesis,
    ( says(b,a_has_lied) )).

cnf(a_from_mars1,hypothesis,
    ( ~ from_mars(a)
    | a_truth(a_from_mars) )).

cnf(a_from_mars2,hypothesis,
    ( from_mars(a)
    | ~ a_truth(a_from_mars) )).

cnf(a_from_mars3,hypothesis,
    ( ~ a_truth(a_from_mars)
    | ~ a_truth(statement_by(b)) )).

cnf(a_states,hypothesis,
    ( statement_by(a) = a_from_mars )).

cnf(b_states,hypothesis,
    ( statement_by(b) = a_has_lied )).

cnf(truth_of_bs_statement,hypothesis,
    ( a_truth(a_from_mars)
    | a_truth(statement_by(b)) )).

cnf(different_sex1,hypothesis,
    ( ~ female(a)
    | male(b) )).

cnf(different_sex2,hypothesis,
    ( ~ male(a)
    | female(b) )).

%----Conclude that they are NOT a mixed couple.
%----Denial of the conclusion: either M(A) & V(B) or V(A) & M(B)
cnf(one_from_mars,negated_conjecture,
    ( from_mars(b)
    | from_mars(a) )).

cnf(one_from_venus,negated_conjecture,
    ( from_venus(a)
    | from_venus(b) )).

%----Two clauses deleted due to tautology: M(A) | V(A), V(B) | M(B)
%--------------------------------------------------------------------------
