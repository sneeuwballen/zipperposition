%--------------------------------------------------------------------------
% File     : PUZ001-3 : TPTP v5.2.0. Released v1.2.0.
% Domain   : Puzzles
% Problem  : Dreadbury Mansion
% Version  : Especial.
% English  : Someone who lives in Dreadbury Mansion killed Aunt Agatha.
%            Agatha, the butler, and Charles live in Dreadbury Mansion,
%            and are the only people who live therein. A killer always
%            hates his victim, and is never richer than his victim.
%            Charles hates no one that Aunt Agatha hates. Agatha hates
%            everyone except the butler. The butler hates everyone not
%            richer than Aunt Agatha. The butler hates everyone Aunt
%            Agatha hates. No one hates everyone. Agatha is not the
%            butler. Therefore : Agatha killed herself.

% Refs     : [Pel86] Pelletier (1986), Seventy-five Problems for Testing Au
%          : [MB88]  Manthey & Bry (1988), SATCHMO: A Theorem Prover Implem
% Source   : [MB88]
% Names    : - [MB88]

% Status   : Satisfiable
% Rating   : 0.00 v2.1.0, 0.00 v2.0.0
% Syntax   : Number of clauses     :   12 (   2 non-Horn;   5 unit;  12 RR)
%            Number of atoms       :   22 (   0 equality)
%            Maximal clause size   :    3 (   2 average)
%            Number of predicates  :    4 (   0 propositional; 1-2 arity)
%            Number of functors    :    3 (   3 constant; 0-0 arity)
%            Number of variables   :    8 (   0 singleton)
%            Maximal term depth    :    1 (   1 average)
% SPC      : CNF_SAT_EPR

% Comments : Also known as "Who killed Aunt Agatha"
%--------------------------------------------------------------------------
cnf(agatha,axiom,
    ( lives(agatha) )).

cnf(butler,axiom,
    ( lives(butler) )).

cnf(charles,axiom,
    ( lives(charles) )).

cnf(poorer_killer,axiom,
    ( ~ killed(X,Y)
    | ~ richer(X,Y) )).

cnf(different_hates,axiom,
    ( ~ hates(agatha,X)
    | ~ hates(charles,X) )).

cnf(no_one_hates_everyone,axiom,
    ( ~ hates(X,agatha)
    | ~ hates(X,butler)
    | ~ hates(X,charles) )).

cnf(agatha_hates_agatha,axiom,
    ( hates(agatha,agatha) )).

cnf(agatha_hates_charles,axiom,
    ( hates(agatha,charles) )).

cnf(killer_hates_victim,axiom,
    ( ~ killed(X,Y)
    | hates(X,Y) )).

cnf(same_hates,axiom,
    ( ~ hates(agatha,X)
    | hates(butler,X) )).

cnf(butler_hates_poor,axiom,
    ( ~ lives(X)
    | richer(X,agatha)
    | hates(butler,X) )).

cnf(somebody_did_it,negated_conjecture,
    ( killed(agatha,agatha)
    | killed(butler,agatha)
    | killed(charles,agatha) )).

%--------------------------------------------------------------------------
