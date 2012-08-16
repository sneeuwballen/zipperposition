%------------------------------------------------------------------------------
% File     : PUZ018_1 : TPTP v5.2.0. Bugfixed v5.0.0.
% Domain   : Puzzles
% Problem  : The Interns
% Version  : Especial.
% English  : Three interns are residents of the same hospital. On only one day
%            of the week are all three interns on call. No intern is on call on
%            three consecutiveutive days. No two interns are off on the same
%            day more than once a week. The first intern is off on Sunday,
%            Tuesday, and Thursday. The second intern is off on Thursday and
%            Saturday. The third intern is off on Sunday.  Which day of the
%            week are all three interns on call?

% Refs     : 
% Source   : [TPTP]
% Names    : 

% Status   : Theorem
% Rating   : 0.00 v5.0.0
% Syntax   : Number of formulae    :   65 (  52 unit;  17 type)
%            Number of atoms       :   98 (   0 equality)
%            Maximal formula depth :   12 (   3 average)
%            Number of connectives :   55 (  31   ~;  11   |;   8   &)
%                                         (   0 <=>;   5  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&)
%            Number of type conns  :    9 (   5   >;   4   *;   0   +;   0  <<)
%            Number of predicates  :   24 (  19 propositional; 0-2 arity)
%            Number of functors    :   10 (  10 constant; 0-0 arity)
%            Number of variables   :   17 (   0 sgn;  17   !;   0   ?)
%            Maximal term depth    :    1 (   1 average)
% SPC      : TFF_THM_NEQ_NAR

% Comments : 
%------------------------------------------------------------------------------
tff(day_type,type,(
    day: $tType )).

tff(person_type,type,(
    person: $tType )).

tff(all_on_type,type,(
    all_on: day > $o )).

tff(on_type,type,(
    on: ( person * day ) > $o )).

tff(consecutive_type,type,(
    consecutive: ( day * day ) > $o )).

tff(same_day_type,type,(
    same_day: ( day * day ) > $o )).

tff(same_person_type,type,(
    same_person: ( person * person ) > $o )).

tff(a_type,type,(
    a: person )).

tff(b_type,type,(
    b: person )).

tff(c_type,type,(
    c: person )).

tff(monday_type,type,(
    monday: day )).

tff(tuesday_type,type,(
    tuesday: day )).

tff(wednesday_type,type,(
    wednesday: day )).

tff(thursday_type,type,(
    thursday: day )).

tff(friday_type,type,(
    friday: day )).

tff(sunday_type,type,(
    sunday: day )).

tff(saturday_type,type,(
    saturday: day )).

tff(all_on_a_on,axiom,(
    ! [X: day] :
      ( all_on(X)
     => on(a,X) ) )).

tff(all_on_b_on,axiom,(
    ! [X: day] :
      ( all_on(X)
     => on(b,X) ) )).

tff(all_on_c_on,axiom,(
    ! [X: day] :
      ( all_on(X)
     => on(c,X) ) )).

tff(all_on,axiom,(
    ! [X: day] :
      ( ( on(a,X)
        & on(b,X)
        & on(c,X) )
     => all_on(X) ) )).

tff(all_on_well_defined,axiom,(
    ! [X: day,Y: day] :
      ( ( all_on(X)
        & all_on(Y) )
     => same_day(X,Y) ) )).

tff(monday_follows_sunday,axiom,(
    consecutive(sunday,monday) )).

tff(tuesday_follows_monday,axiom,(
    consecutive(monday,tuesday) )).

tff(wednesday_follows_tuesday,axiom,(
    consecutive(tuesday,wednesday) )).

tff(thursday_follows_wednesday,axiom,(
    consecutive(wednesday,thursday) )).

tff(friday_follows_thursday,axiom,(
    consecutive(thursday,friday) )).

tff(staurday_follows_friday,axiom,(
    consecutive(friday,saturday) )).

tff(sunday_follows_saturday,axiom,(
    consecutive(saturday,sunday) )).

tff(reflexivity_for_same_person,axiom,(
    ! [X: person] : same_person(X,X) )).

tff(a_not_b,axiom,(
    ~ same_person(a,b) )).

tff(a_not_c,axiom,(
    ~ same_person(a,c) )).

tff(b_not_c,axiom,(
    ~ same_person(b,c) )).

tff(reflexivity_for_same_day,axiom,(
    ! [X: day] : same_day(X,X) )).

tff(sunday_not_monday,axiom,(
    ~ same_day(sunday,monday) )).

tff(sunday_not_tuesday,axiom,(
    ~ same_day(sunday,tuesday) )).

tff(sunday_not_wednesday,axiom,(
    ~ same_day(sunday,wednesday) )).

tff(sunday_not_thursday,axiom,(
    ~ same_day(sunday,thursday) )).

tff(sunday_not_friday,axiom,(
    ~ same_day(sunday,friday) )).

tff(sunday_not_saturday,axiom,(
    ~ same_day(sunday,saturday) )).

tff(monday_not_tuesday,axiom,(
    ~ same_day(monday,tuesday) )).

tff(monday_not_wednesday,axiom,(
    ~ same_day(monday,wednesday) )).

tff(monday_not_thursday,axiom,(
    ~ same_day(monday,thursday) )).

tff(monday_not_friday,axiom,(
    ~ same_day(monday,friday) )).

tff(monday_not_saturday,axiom,(
    ~ same_day(monday,saturday) )).

tff(tuesday_not_wednesday,axiom,(
    ~ same_day(tuesday,wednesday) )).

tff(tuesday_not_thursday,axiom,(
    ~ same_day(tuesday,thursday) )).

tff(tuesday_not_friday,axiom,(
    ~ same_day(tuesday,friday) )).

tff(tuesday_not_saturday,axiom,(
    ~ same_day(tuesday,saturday) )).

tff(wednesday_not_thursday,axiom,(
    ~ same_day(wednesday,thursday) )).

tff(wednesday_not_friday,axiom,(
    ~ same_day(wednesday,friday) )).

tff(wednesday_not_saturday,axiom,(
    ~ same_day(wednesday,saturday) )).

tff(thursday_not_friday,axiom,(
    ~ same_day(thursday,friday) )).

tff(thursday_not_saturday,axiom,(
    ~ same_day(thursday,saturday) )).

tff(friday_not_saturday,axiom,(
    ~ same_day(friday,saturday) )).

tff(all_on_one_day,hypothesis,
    ( all_on(sunday)
    | all_on(monday)
    | all_on(tuesday)
    | all_on(wednesday)
    | all_on(thursday)
    | all_on(friday)
    | all_on(saturday) )).

tff(not_on_for_3_days,hypothesis,(
    ! [X: day,Y: day,Z: day,W: day,U: person] :
      ~ ( consecutive(X,Y)
        & consecutive(Y,Z)
        & consecutive(Z,W)
        & on(U,X)
        & on(U,Y)
        & on(U,Z) ) )).

tff(no_two_off_twice_together,hypothesis,(
    ! [X: person,Y: day,Z: day,W: person] :
      ( on(X,Y)
      | on(X,Z)
      | on(W,Y)
      | on(W,Z)
      | same_person(X,W)
      | same_day(Y,Z) ) )).

tff(a_off_sunday,hypothesis,(
    ~ on(a,sunday) )).

tff(a_off_tuesday,hypothesis,(
    ~ on(a,tuesday) )).

tff(a_off_thursday,hypothesis,(
    ~ on(a,thursday) )).

tff(b_off_thursday,hypothesis,(
    ~ on(b,thursday) )).

tff(b_off_saturday,hypothesis,(
    ~ on(b,saturday) )).

tff(c_off_sunday,hypothesis,(
    ~ on(c,sunday) )).

tff(prove_all_on_friday,conjecture,(
    all_on(friday) )).
%------------------------------------------------------------------------------
