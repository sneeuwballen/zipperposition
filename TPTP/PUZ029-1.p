%--------------------------------------------------------------------------
% File     : PUZ029-1 : TPTP v5.2.0. Released v1.0.0.
% Domain   : Puzzles
% Problem  : The pigs and balloons puzzle
% Version  : Especial.
% English  : 1) All, who neither dance on tight ropes nor eat penny-buns,
%               are old.
%            2) Pigs, that are liable to giddiness, are treated with respect.
%            3) A wise balloonist takes an umbrella with him.
%            4) No one ought to lunch in public who looks ridiculous and eats
%               penny-buns.
%            5) Young creatures, who go up in balloons, are liable to
%               giddiness.
%            6) Fat creatures, who look ridiculous, may lunch in public,
%               provided that they do not dance on tight ropes.
%            7) No wise creatures dance on tight ropes, if liable to
%               giddiness.
%            8) A pig looks ridiculous, carrying an umbrella.
%            9) All, who do not dance on tight ropes, and who are treated
%               with respect are fat.
%            Show that no wise young pigs go up in balloons.

% Refs     : [Car86] Carroll (1986), Lewis Carroll's Symbolic Logic
% Source   : [ANL]
% Names    : pigs.ver1.in [ANL]

% Status   : Unsatisfiable
% Rating   : 0.00 v2.0.0
% Syntax   : Number of clauses     :   15 (   4 non-Horn;   4 unit;  13 RR)
%            Number of atoms       :   36 (   0 equality)
%            Maximal clause size   :    4 (   2 average)
%            Number of predicates  :   13 (   0 propositional; 1-1 arity)
%            Number of functors    :    1 (   1 constant; 0-0 arity)
%            Number of variables   :   11 (   0 singleton)
%            Maximal term depth    :    1 (   1 average)
% SPC      : CNF_UNS_EPR

% Comments :
%--------------------------------------------------------------------------
cnf(boring_old_people,axiom,
    ( dances_on_tightropes(X)
    | eats_pennybuns(X)
    | old(X) )).

cnf(giddy_pigs_reated_with_respect,axiom,
    ( ~ pig(X)
    | ~ liable_to_giddiness(X)
    | treated_with_respect(X) )).

cnf(wise_ballonists_have_umbrellas,axiom,
    ( ~ wise(X)
    | ~ balloonist(X)
    | has_umbrella(X) )).

cnf(dont_look_ridiculous_eating_buns_in_public,axiom,
    ( ~ looks_ridiculous(X)
    | ~ eats_pennybuns(X)
    | ~ eats_lunch_in_public(X) )).

cnf(young_balloonists_get_giddy,axiom,
    ( ~ balloonist(X)
    | ~ young(X)
    | liable_to_giddiness(X) )).

cnf(fat_ridiculous_off_tightrope_eat_in_public,axiom,
    ( ~ fat(X)
    | ~ looks_ridiculous(X)
    | dances_on_tightropes(X)
    | eats_lunch_in_public(X) )).

cnf(wise_giddy_dont_dance_on_tightrope,axiom,
    ( ~ liable_to_giddiness(X)
    | ~ wise(X)
    | ~ dances_on_tightropes(X) )).

cnf(pigs_look_ridiculous_with_umbrellas,axiom,
    ( ~ pig(X)
    | ~ has_umbrella(X)
    | looks_ridiculous(X) )).

cnf(non_dancers_who_are_respected_are_fat,axiom,
    ( dances_on_tightropes(X)
    | ~ treated_with_respect(X)
    | fat(X) )).

cnf(young_or_old,axiom,
    ( young(X)
    | old(X) )).

cnf(not_young_and_old,axiom,
    ( ~ young(X)
    | ~ old(X) )).

cnf(piggy_is_wise,hypothesis,
    ( wise(piggy) )).

cnf(piggy_is_young,hypothesis,
    ( young(piggy) )).

cnf(piggy_is_a_pig,hypothesis,
    ( pig(piggy) )).

cnf(prove_piggy_is_no_balloonist,negated_conjecture,
    ( balloonist(piggy) )).

%--------------------------------------------------------------------------
