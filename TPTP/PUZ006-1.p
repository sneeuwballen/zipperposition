%--------------------------------------------------------------------------
% File     : PUZ006-1 : TPTP v5.2.0. Released v1.0.0.
% Domain   : Puzzles
% Problem  : Determine sex and race on Mars and Venus
% Version  : Especial.
% English  : Here's the situation: human observers in this exclusive club
%            on Ganymede can't distinguish Martians from Venusians, males
%            from females, except for the fact that Venusian women and
%            Martian men always tell the truth and Venusian men and
%            Martian women always lie.
%            Ork says "Bog is from Venus."  Bog says "Ork is from Mars."
%            Ork says "Bog is male."  Bog says "Ork is female." Who's
%            what?  (sex & race).

% Refs     :
% Source   : [ANL]
% Names    : mars_venus.in [ANL]

% Status   : Unsatisfiable
% Rating   : 0.17 v5.2.0, 0.12 v5.1.0, 0.06 v5.0.0, 0.00 v3.3.0, 0.07 v3.2.0, 0.00 v3.1.0, 0.09 v2.7.0, 0.08 v2.6.0, 0.00 v2.2.0, 0.11 v2.1.0, 0.00 v2.0.0
% Syntax   : Number of clauses     :   29 (   4 non-Horn;   5 unit;  25 RR)
%            Number of atoms       :   60 (   1 equality)
%            Maximal clause size   :    3 (   2 average)
%            Number of predicates  :    9 (   0 propositional; 1-2 arity)
%            Number of functors    :    7 (   6 constant; 0-1 arity)
%            Number of variables   :   20 (   1 singleton)
%            Maximal term depth    :    2 (   1 average)
% SPC      : CNF_UNS_RFO_SEQ_NHN

% Comments :
%--------------------------------------------------------------------------
%----Include Mars and Venus axioms
include('Axioms/PUZ001-0.ax').
%--------------------------------------------------------------------------
%----Ork says "Bog is from Venus."  Bog says "Ork is from Mars." Ork says
%----"Bog is male."  Bog says "Ork is female.
cnf(ork_says_bog_is_from_venus,hypothesis,
    ( says(ork,bog_is_from_venus) )).

cnf(bog_says_ork_is_from_mar,hypothesis,
    ( says(bog,ork_is_from_mars) )).

cnf(ork_says_bog_is_male,hypothesis,
    ( says(ork,bog_is_male) )).

cnf(bog_says_ork_is_female,hypothesis,
    ( says(bog,ork_is_female) )).

cnf(bog_is_from_venus1,hypothesis,
    ( ~ a_truth(bog_is_from_venus)
    | from_venus(bog) )).

cnf(ork_is_from_mars1,hypothesis,
    ( ~ a_truth(ork_is_from_mars)
    | from_mars(ork) )).

cnf(bog_is_male1,hypothesis,
    ( ~ a_truth(bog_is_male)
    | male(bog) )).

cnf(ork_is_female1,hypothesis,
    ( ~ a_truth(ork_is_female)
    | female(ork) )).

cnf(bog_is_from_venus2,hypothesis,
    ( ~ from_venus(bog)
    | a_truth(bog_is_from_venus) )).

cnf(ork_is_from_mars2,hypothesis,
    ( ~ from_mars(ork)
    | a_truth(ork_is_from_mars) )).

cnf(bog_is_male2,hypothesis,
    ( ~ male(bog)
    | a_truth(bog_is_male) )).

cnf(ork_is_female2,hypothesis,
    ( ~ female(ork)
    | a_truth(ork_is_female) )).

cnf(prove_bog_is_female,negated_conjecture,
    ( ~ female(bog) )).

%--------------------------------------------------------------------------
