%------------------------------------------------------------------------------
% File     : PUZ135_2 : TPTP v5.2.0. Released v5.1.0.
% Domain   : Puzzles
% Problem  : The Knowheyan Art Fair Puzzle - Order
% Version  : Especial.
% English  : Knowheyans are not negative by nature; it is just that their way
%            of expressing themselves, using negative sentences, makes it 
%            appear that way. Several fortunate visitors have an opportunity
%            to attend the Midseason Art Fair, in which they are able to 
%            observe a variety of Knowheyans art forms. An artists' competition
%            was held as part of the fair and awards were given for the top
%            four entries, which were collage painting, holography, laser
%            etchings, and reconstituted materials sculpture. The interpreter
%            is explaining the results of the competition to the visitors:
%            1. A, who was not the first-place winner, did not enter a 
%               holograph.
%            2. The fourth-place winner did not enter a sculpture or a 
%               holograph.
%            3. The one who entered the collage painting, who was neither A
%               nor C, did not win first or second place.
%            4. Neither B, nor the one who entered the laser etching, was the
%               fourth-place winner.
%            5. The third-place winner was neither B nor C.
%            6. The one who entered the laser etching, who was not A, was not
%               the second place winner.
%            In what order did they finish?

% Refs     : [WS+06] Willis et al. (2006), The World's Biggest Book of Brai
% Source   : [TPTP]
% Names    :

% Status   : Theorem
% Rating   : 0.00 v5.2.0, 0.33 v5.1.0
% Syntax   : Number of formulae    :   47 (  33 unit;  17 type)
%            Number of atoms       :   76 (  57 equality)
%            Maximal formula depth :    7 (   3 average)
%            Number of connectives :   64 (  37   ~;   9   |;  12   &)
%                                         (   0 <=>;   6  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&)
%            Number of type conns  :    2 (   2   >;   0   *;   0   +;   0  <<)
%            Number of predicates  :   19 (  18 propositional; 0-2 arity)
%            Number of functors    :   14 (  12 constant; 0-1 arity)
%            Number of variables   :   11 (   0 sgn;  11   !;   0   ?)
%            Maximal term depth    :    2 (   1 average)
% SPC      : TFF_THM_EQU_NAR

% Comments :
%------------------------------------------------------------------------------
tff(knowheyan_type,type,(
    knowheyan: $tType )).

tff(place_type,type,(
    place: $tType )).

tff(entry_type,type,(
    entry: $tType )).

tff(entry_of_type,type,(
    entry_of: knowheyan > entry )).

tff(place_of_type,type,(
    place_of: knowheyan > place )).

tff(a_knowheyan,type,(
    a: knowheyan )).

tff(b_knowheyan,type,(
    b: knowheyan )).

tff(c_knowheyan,type,(
    c: knowheyan )).

tff(d_knowheyan,type,(
    d: knowheyan )).

tff(a_not_b,axiom,(
    a != b )).

tff(a_not_c,axiom,(
    a != c )).

tff(a_not_d,axiom,(
    a != d )).

tff(b_not_c,axiom,(
    b != c )).

tff(b_not_d,axiom,(
    b != d )).

tff(c_not_d,axiom,(
    c != d )).

tff(collage_painting_entry,type,(
    collage_painting: entry )).

tff(holography_entry,type,(
    holography: entry )).

tff(laser_etching_entry,type,(
    laser_etching: entry )).

tff(reconstituted_materials_sculpture_entry,type,(
    reconstituted_materials_sculpture: entry )).

tff(collage_painting_not_holography,axiom,(
    collage_painting != holography )).

tff(collage_painting_not_laser_etching,axiom,(
    collage_painting != laser_etching )).

tff(collage_painting_not_reconstituted_materials_sculpture,axiom,(
    collage_painting != reconstituted_materials_sculpture )).

tff(holography_not_laser_etching,axiom,(
    holography != laser_etching )).

tff(holography_not_reconstituted_materials_sculpture,axiom,(
    holography != reconstituted_materials_sculpture )).

tff(laser_etching_not_reconstituted_materials_sculpture,axiom,(
    laser_etching != reconstituted_materials_sculpture )).

tff(first_place,type,(
    first: place )).

tff(second_place,type,(
    second: place )).

tff(third_place,type,(
    third: place )).

tff(fourth_place,type,(
    fourth: place )).

tff(first_not_second,axiom,(
    first != second )).

tff(first_not_third,axiom,(
    first != third )).

tff(first_not_fourth,axiom,(
    first != fourth )).

tff(second_not_third,axiom,(
    second != third )).

tff(second_not_fourth,axiom,(
    second != fourth )).

tff(third_not_fourth,axiom,(
    third != fourth )).

tff(only_knowheyans,axiom,(
    ! [X: knowheyan] :
      ( X = a
      | X = b
      | X = c
      | X = d ) )).

tff(only_entries,axiom,(
    ! [X: entry] :
      ( X = collage_painting
      | X = holography
      | X = laser_etching
      | X = reconstituted_materials_sculpture ) )).

tff(only_places,axiom,(
    ! [X: place] :
      ( X = first
      | X = second
      | X = third
      | X = fourth ) )).

tff(unique_entrys,axiom,(
    ! [X: knowheyan,Y: knowheyan] :
      ( X != Y
     => entry_of(X) != entry_of(Y) ) )).

tff(unique_places,axiom,(
    ! [X: knowheyan,Y: knowheyan] :
      ( X != Y
     => place_of(X) != place_of(Y) ) )).

tff(a_info,axiom,
    ( place_of(a) != first
    & entry_of(a) != holography )).

tff(fourth_info,axiom,(
    ! [X: knowheyan] :
      ( place_of(X) = fourth
     => ( entry_of(X) != reconstituted_materials_sculpture
        & entry_of(X) != holography ) ) )).

tff(collage_info,axiom,(
    ! [X: knowheyan] :
      ( entry_of(X) = collage_painting
     => ( X != a
        & X != c
        & place_of(X) != first
        & place_of(X) != second ) ) )).

tff(fourth_info2,axiom,
    ( place_of(b) != fourth
    & ! [X: knowheyan] :
        ( entry_of(X) = laser_etching
       => place_of(X) != fourth ) )).

tff(third_info,axiom,
    ( place_of(b) != third
    & place_of(c) != third )).

tff(laser_etching_info,axiom,
    ( entry_of(b) != laser_etching
    & ! [X: knowheyan] :
        ( entry_of(X) = laser_etching
       => ( X != a
          & place_of(X) != second ) ) )).

tff(places,conjecture,
    ( place_of(a) = third
    & place_of(b) = second
    & place_of(c) = first
    & place_of(d) = fourth )).

%------------------------------------------------------------------------------
