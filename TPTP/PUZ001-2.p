%--------------------------------------------------------------------------
% File     : PUZ001-2 : TPTP v5.2.0. Released v1.0.0.
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
%          : [Pel88] Pelletier (1988), Errata
% Source   : [Pel86]
% Names    : Pelletier 55 [Pel86]

% Status   : Unsatisfiable
% Rating   : 0.11 v5.2.0, 0.00 v5.1.0, 0.06 v5.0.0, 0.07 v4.1.0, 0.08 v4.0.1, 0.09 v3.7.0, 0.00 v3.4.0, 0.08 v3.3.0, 0.21 v3.2.0, 0.15 v3.1.0, 0.18 v2.7.0, 0.17 v2.6.0, 0.10 v2.5.0, 0.17 v2.4.0, 0.00 v2.3.0, 0.11 v2.2.1, 0.11 v2.2.0, 0.11 v2.1.0, 0.00 v2.0.0
% Syntax   : Number of clauses     :   15 (   3 non-Horn;   8 unit;  13 RR)
%            Number of atoms       :   24 (   5 equality)
%            Maximal clause size   :    4 (   2 average)
%            Number of predicates  :    5 (   0 propositional; 1-2 arity)
%            Number of functors    :    5 (   4 constant; 0-1 arity)
%            Number of variables   :   10 (   0 singleton)
%            Maximal term depth    :    2 (   1 average)
% SPC      : CNF_UNS_RFO_SEQ_NHN

% Comments : Also known as "Who killed Aunt Agatha"
%          : This problem was contributed to [Pel86] by Len Schubert.
%          : Schubert says "I don't know of any other places where my
%            "Dreadbury Mansion" (or "Aunt Agatha") problem has been
%            published, besides in Jeff's "75 Problems". I formulated the
%            problem as an exercise in resolution, paramodulation and
%            answer extraction for a graduate AI class at the Univ.
%            of Alberta in 1983."
%          : This problem is incorrect in [Pel86] and is corrected in [Pel88].
%--------------------------------------------------------------------------
%----The axioms of the problem
cnf(someone_in_mansion,axiom,
    ( lives_at_dreadsbury(someone) )).

cnf(someone_killed_agatha,axiom,
    ( killed(someone,aunt_agatha) )).

cnf(agatha_lives_at_mansion,axiom,
    ( lives_at_dreadsbury(aunt_agatha) )).

cnf(butler_lives_at_mansion,axiom,
    ( lives_at_dreadsbury(butler) )).

cnf(charles_lives_at_mansion,axiom,
    ( lives_at_dreadsbury(charles) )).

cnf(noone_else_lives_at_mansion,axiom,
    ( ~ lives_at_dreadsbury(Person)
    | Person = aunt_agatha
    | Person = butler
    | Person = charles )).

cnf(killer_hates_victim,axiom,
    ( ~ killed(Killer,Victim)
    | hates(Killer,Victim) )).

cnf(killer_poorer_than_victim,axiom,
    ( ~ killed(Killer,Victim)
    | ~ richer(Killer,Victim) )).

cnf(charles_and_agatha_hate_different_people,axiom,
    ( ~ hates(aunt_agatha,Person)
    | ~ hates(charles,Person) )).

cnf(agatha_likes_only_butler,axiom,
    ( Person = butler
    | hates(aunt_agatha,Person) )).

cnf(butler_hates_poor_people,axiom,
    ( richer(Person,aunt_agatha)
    | hates(butler,Person) )).

cnf(butler_and_agatha_hate_the_same_people,axiom,
    ( ~ hates(aunt_agatha,Person)
    | hates(butler,Person) )).

cnf(noone_hates_everyone,axiom,
    ( ~ hates(Person,every_one_but(Person)) )).

cnf(agatha_is_not_the_butler,axiom,
    (  aunt_agatha != butler )).

cnf(prove_agatha_killed_herself,negated_conjecture,
    ( ~ killed(aunt_agatha,aunt_agatha) )).

%--------------------------------------------------------------------------
