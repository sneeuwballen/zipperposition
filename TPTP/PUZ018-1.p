%--------------------------------------------------------------------------
% File     : PUZ018-1 : TPTP v5.2.0. Bugfixed v1.2.0.
% Domain   : Puzzles
% Problem  : The Interns
% Version  : Especial.
% English  : Three interns are residents of the same hospital. On only one
%            day of the week are all three interns on call. No intern
%            is on call on three consecutiveutive days. No two interns are
%            off on the same day more than once a week. The first intern
%            is off on Sunday, Tuesday, and Thursday. The second intern
%            is off on Thursday and Saturday. The third intern is off
%            on Sunday.  Which day of the week are all three interns
%            on call?

% Refs     : [Rap95] Raptis (1995), Email to G. Sutcliffe
% Source   : [ANL]
% Names    : interns.ver1.in [ANL]

% Status   : Unsatisfiable
% Rating   : 0.00 v3.1.0, 0.11 v2.7.0, 0.17 v2.6.0, 0.11 v2.5.0, 0.25 v2.4.0, 0.00 v2.1.0, 0.00 v2.0.0
% Syntax   : Number of clauses     :   48 (   2 non-Horn;  40 unit;  45 RR)
%            Number of atoms       :   72 (   0 equality)
%            Maximal clause size   :    7 (   2 average)
%            Number of predicates  :    5 (   0 propositional; 1-2 arity)
%            Number of functors    :   10 (  10 constant; 0-0 arity)
%            Number of variables   :   17 (   1 singleton)
%            Maximal term depth    :    1 (   1 average)
% SPC      : CNF_UNS_EPR

% Comments : Dimitris Raptis pointed out [Rap95] that this was trivially
%            unsatisfiable, by resolving prove_all_on_one_day
%            [--all_on(Day)] with all_on_one_day.
% Bugfixes : v1.2.0 - Theorem clause prove_all_on_one_day grounded.
%--------------------------------------------------------------------------
cnf(all_on_a_on,axiom,
    ( ~ all_on(X)
    | on(a,X) )).

cnf(all_on_b_on,axiom,
    ( ~ all_on(X)
    | on(b,X) )).

cnf(all_on_c_on,axiom,
    ( ~ all_on(X)
    | on(c,X) )).

cnf(all_on,axiom,
    ( all_on(X)
    | ~ on(a,X)
    | ~ on(b,X)
    | ~ on(c,X) )).

cnf(all_on_well_defined,axiom,
    ( ~ all_on(X)
    | ~ all_on(Y)
    | same_day(X,Y) )).

cnf(monday_follows_sunday,axiom,
    ( consecutive(sunday,monday) )).

cnf(tuesday_follows_monday,axiom,
    ( consecutive(monday,tuesday) )).

cnf(wednesday_follows_tuesday,axiom,
    ( consecutive(tuesday,wednesday) )).

cnf(thursday_follows_wednesday,axiom,
    ( consecutive(wednesday,thursday) )).

cnf(friday_follows_thursday,axiom,
    ( consecutive(thursday,friday) )).

cnf(staurday_follows_friday,axiom,
    ( consecutive(friday,saturday) )).

cnf(sunday_follows_saturday,axiom,
    ( consecutive(saturday,sunday) )).

cnf(reflexivity_for_same_person,axiom,
    ( same_person(X,X) )).

cnf(a_not_b,axiom,
    ( ~ same_person(a,b) )).

cnf(a_not_c,axiom,
    ( ~ same_person(a,c) )).

cnf(b_not_c,axiom,
    ( ~ same_person(b,c) )).

cnf(reflexivity_for_same_day,axiom,
    ( same_day(X,X) )).

cnf(sunday_not_monday,axiom,
    ( ~ same_day(sunday,monday) )).

cnf(sunday_not_tuesday,axiom,
    ( ~ same_day(sunday,tuesday) )).

cnf(sunday_not_wednesday,axiom,
    ( ~ same_day(sunday,wednesday) )).

cnf(sunday_not_thursday,axiom,
    ( ~ same_day(sunday,thursday) )).

cnf(sunday_not_friday,axiom,
    ( ~ same_day(sunday,friday) )).

cnf(sunday_not_saturday,axiom,
    ( ~ same_day(sunday,saturday) )).

cnf(monday_not_tuesday,axiom,
    ( ~ same_day(monday,tuesday) )).

cnf(monday_not_wednesday,axiom,
    ( ~ same_day(monday,wednesday) )).

cnf(monday_not_thursday,axiom,
    ( ~ same_day(monday,thursday) )).

cnf(monday_not_friday,axiom,
    ( ~ same_day(monday,friday) )).

cnf(monday_not_saturday,axiom,
    ( ~ same_day(monday,saturday) )).

cnf(tuesday_not_wednesday,axiom,
    ( ~ same_day(tuesday,wednesday) )).

cnf(tuesday_not_thursday,axiom,
    ( ~ same_day(tuesday,thursday) )).

cnf(tuesday_not_friday,axiom,
    ( ~ same_day(tuesday,friday) )).

cnf(tuesday_not_saturday,axiom,
    ( ~ same_day(tuesday,saturday) )).

cnf(wednesday_not_thursday,axiom,
    ( ~ same_day(wednesday,thursday) )).

cnf(wednesday_not_friday,axiom,
    ( ~ same_day(wednesday,friday) )).

cnf(wednesday_not_saturday,axiom,
    ( ~ same_day(wednesday,saturday) )).

cnf(thursday_not_friday,axiom,
    ( ~ same_day(thursday,friday) )).

cnf(thursday_not_saturday,axiom,
    ( ~ same_day(thursday,saturday) )).

cnf(friday_not_saturday,axiom,
    ( ~ same_day(friday,saturday) )).

cnf(all_on_one_day,hypothesis,
    ( all_on(sunday)
    | all_on(monday)
    | all_on(tuesday)
    | all_on(wednesday)
    | all_on(thursday)
    | all_on(friday)
    | all_on(saturday) )).

cnf(not_on_for_3_days,hypothesis,
    ( ~ consecutive(X,Y)
    | ~ consecutive(Y,Z)
    | ~ consecutive(Z,W)
    | ~ on(U,X)
    | ~ on(U,Y)
    | ~ on(U,Z) )).

cnf(no_two_off_twice_together,hypothesis,
    ( on(X,Y)
    | on(X,Z)
    | on(W,Y)
    | on(W,Z)
    | same_person(X,W)
    | same_day(Y,Z) )).

cnf(a_off_sunday,hypothesis,
    ( ~ on(a,sunday) )).

cnf(a_off_tuesday,hypothesis,
    ( ~ on(a,tuesday) )).

cnf(a_off_thursday,hypothesis,
    ( ~ on(a,thursday) )).

cnf(b_off_thursday,hypothesis,
    ( ~ on(b,thursday) )).

cnf(b_off_saturday,hypothesis,
    ( ~ on(b,saturday) )).

cnf(c_off_sunday,hypothesis,
    ( ~ on(c,sunday) )).

cnf(prove_all_on_friday,negated_conjecture,
    ( ~ all_on(friday) )).

%--------------------------------------------------------------------------
