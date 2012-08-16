%--------------------------------------------------------------------------
% File     : PUZ022-1 : TPTP v5.2.0. Released v1.1.0.
% Domain   : Puzzles
% Problem  : An ocean that borders on two adjacent Australian states
% Version  : Especial.
% English  : There is a database of assertions about Australian states and
%            oceans and their relationships. Find which ocean borders
%            on two adjacent Australian states.

% Refs     : [Pla82] Plaisted (1982), A Simplified Problem Reduction Format
% Source   : [TPTP]
% Names    :

% Status   : Unsatisfiable
% Rating   : 0.00 v2.0.0
% Syntax   : Number of clauses     :   33 (   0 non-Horn;  31 unit;  33 RR)
%            Number of atoms       :   39 (   0 equality)
%            Maximal clause size   :    6 (   1 average)
%            Number of predicates  :    3 (   0 propositional; 1-2 arity)
%            Number of functors    :   11 (  11 constant; 0-0 arity)
%            Number of variables   :    5 (   0 singleton)
%            Maximal term depth    :    1 (   1 average)
% SPC      : CNF_UNS_EPR

% Comments : Inspired by Problem 5.6 in [Pla82]
%--------------------------------------------------------------------------
cnf(symmetry_of_borders,axiom,
    ( ~ borders(Y,X)
    | borders(X,Y) )).

cnf(atlantic,negated_conjecture,
    ( ocean(atlantic) )).

cnf(indian,negated_conjecture,
    ( ocean(indian) )).

cnf(pacific,negated_conjecture,
    ( ocean(pacific) )).

cnf(southern,negated_conjecture,
    ( ocean(southern) )).

cnf(western_australia,negated_conjecture,
    ( state(western_australia) )).

cnf(northern_territory,negated_conjecture,
    ( state(northern_territory) )).

cnf(queensland,negated_conjecture,
    ( state(queensland) )).

cnf(south_australia,negated_conjecture,
    ( state(south_australia) )).

cnf(new_south_wales,negated_conjecture,
    ( state(new_south_wales) )).

cnf(victoria,negated_conjecture,
    ( state(victoria) )).

cnf(tasmania,negated_conjecture,
    ( state(tasmania) )).

cnf(wa_nt,negated_conjecture,
    ( borders(western_australia,northern_territory) )).

cnf(wa_sa,negated_conjecture,
    ( borders(western_australia,south_australia) )).

cnf(sa_nt,negated_conjecture,
    ( borders(south_australia,northern_territory) )).

cnf(sa_qld,negated_conjecture,
    ( borders(south_australia,queensland) )).

cnf(sa_nsw,negated_conjecture,
    ( borders(south_australia,new_south_wales) )).

cnf(sa_vic,negated_conjecture,
    ( borders(south_australia,victoria) )).

cnf(nt_qld,negated_conjecture,
    ( borders(northern_territory,queensland) )).

cnf(qld_nsw,negated_conjecture,
    ( borders(queensland,new_south_wales) )).

cnf(nsw_vic,negated_conjecture,
    ( borders(new_south_wales,victoria) )).

cnf(indian_wa,negated_conjecture,
    ( borders(indian,western_australia) )).

cnf(indian_nt,negated_conjecture,
    ( borders(indian,northern_territory) )).

cnf(indian_qld,negated_conjecture,
    ( borders(indian,queensland) )).

cnf(southern_wa,negated_conjecture,
    ( borders(southern,western_australia) )).

cnf(southern_sa,negated_conjecture,
    ( borders(southern,south_australia) )).

cnf(southern_vic,negated_conjecture,
    ( borders(southern,victoria) )).

cnf(southern_tas,negated_conjecture,
    ( borders(southern,tasmania) )).

cnf(pacific_qld,negated_conjecture,
    ( borders(pacific,queensland) )).

cnf(pacific_nsw,negated_conjecture,
    ( borders(pacific,new_south_wales) )).

cnf(pacific_vic,negated_conjecture,
    ( borders(pacific,victoria) )).

cnf(pacific_tas,negated_conjecture,
    ( borders(pacific,tasmania) )).

cnf(prove_borders,negated_conjecture,
    ( ~ state(State1)
    | ~ state(State2)
    | ~ borders(State1,State2)
    | ~ borders(State1,Ocean)
    | ~ borders(State2,Ocean)
    | ~ ocean(Ocean) )).

%--------------------------------------------------------------------------
