%------------------------------------------------------------------------------
% File     : PUZ078+1 : TPTP v5.2.0. Released v3.5.0.
% Domain   : Puzzles
% Problem  : Leo the Liar
% Version  : Especial.
% English  : Leo the Liar is a strange liar. On six days of the week, he lies.
%            On the seventh day he always tells the truth. He made the
%            following statements on three consecutive days:
%            Day 1. I lie on Monday and Tuesday.
%            Day 2. Today it is Thursday, Saturday, or Sunday.
%            Day 3. I lie on Wednesday and Friday.
%            Prove that Leo tells the truth on Tuesday.

% Refs     : [Hom07] Hommersom (2007), Email to Geoff Sutcliffe
% Source   : [Hom07]
% Names    : leoliar [Hom07]

% Status   : Theorem
% Rating   : 0.30 v5.2.0, 0.20 v5.1.0, 0.24 v5.0.0, 0.21 v4.1.0, 0.26 v4.0.0, 0.29 v3.7.0, 0.35 v3.5.0
% Syntax   : Number of formulae    :   23 (  12 unit)
%            Number of atoms       :   69 (  39 equality)
%            Maximal formula depth :   22 (   4 average)
%            Number of connectives :   71 (  25   ~;   8   |;  26   &)
%                                         (   5 <=>;   7  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&)
%            Number of predicates  :    5 (   0 propositional; 1-2 arity)
%            Number of functors    :    8 (   7 constant; 0-1 arity)
%            Number of variables   :   18 (   0 sgn;  14   !;   4   ?)
%            Maximal term depth    :    2 (   1 average)
% SPC      : FOF_THM_RFO_SEQ

% Comments : From http://www.vierkantvoorwiskunde.nl
%------------------------------------------------------------------------------
fof(days_of_week,axiom,(
    ! [X] :
      ( X = monday
      | X = tuesday
      | X = wednesday
      | X = thursday
      | X = friday
      | X = saturday
      | X = sunday ) )).

fof(uniquenames,axiom,
    ( monday != tuesday
    & monday != wednesday
    & monday != thursday
    & monday != friday
    & monday != saturday
    & monday != sunday
    & tuesday != wednesday
    & tuesday != thursday
    & tuesday != friday
    & tuesday != saturday
    & tuesday != sunday
    & wednesday != thursday
    & wednesday != friday
    & wednesday != saturday
    & wednesday != sunday
    & thursday != friday
    & thursday != saturday
    & thursday != sunday
    & friday != saturday
    & friday != sunday
    & saturday != sunday )).

fof(truth_unique,axiom,(
    ! [X,Y] :
      ( ( truthday(X)
        & truthday(Y) )
     => X = Y ) )).

fof(day1_unique,axiom,(
    ! [X,Y] :
      ( ( day1(X)
        & day1(Y) )
     => X = Y ) )).

fof(day2_unique,axiom,(
    ! [X,Y] :
      ( ( day2(X)
        & day2(Y) )
     => X = Y ) )).

fof(day3_unique,axiom,(
    ! [X,Y] :
      ( ( day3(X)
        & day3(Y) )
     => X = Y ) )).

fof(truth_once,axiom,(
    ? [X] : truthday(X) )).

fof(day1_once,axiom,(
    ? [X] : day1(X) )).

fof(day2_once,axiom,(
    ? [X] : day2(X) )).

fof(day3_once,axiom,(
    ? [X] : day3(X) )).

fof(monday_follows_sunday,axiom,(
    consecutive(sunday) = monday )).

fof(tuesday_follows_monday,axiom,(
    consecutive(monday) = tuesday )).

fof(wednesday_follows_tuesday,axiom,(
    consecutive(tuesday) = wednesday )).

fof(thursday_follows_wednesday,axiom,(
    consecutive(wednesday) = thursday )).

fof(friday_follows_thursday,axiom,(
    consecutive(thursday) = friday )).

fof(saturday_follows_friday,axiom,(
    consecutive(friday) = saturday )).

fof(sunday_follows_saturday,axiom,(
    consecutive(saturday) = sunday )).

fof(day2_follows_day1,axiom,(
    ! [X] :
      ( day1(X)
    <=> day2(consecutive(X)) ) )).

fof(day3_follows_day2,axiom,(
    ! [X] :
      ( day2(X)
    <=> day3(consecutive(X)) ) )).

fof(statement1,axiom,(
    ! [X] :
      ( day1(X)
     => ( truthday(X)
      <=> ( ~ truthday(monday)
          & ~ truthday(tuesday) ) ) ) )).

fof(statement2,axiom,(
    ! [X] :
      ( day2(X)
     => ( truthday(X)
      <=> ( day2(thursday)
          | day2(saturday)
          | day2(sunday) ) ) ) )).

fof(statement3,axiom,(
    ! [X] :
      ( day3(X)
     => ( truthday(X)
      <=> ( ~ truthday(wednesday)
          & ~ truthday(friday) ) ) ) )).

fof(tuesday_leo_tells_truths,conjecture,(
    truthday(tuesday) )).

%------------------------------------------------------------------------------
