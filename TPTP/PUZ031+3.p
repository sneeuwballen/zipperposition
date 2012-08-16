%------------------------------------------------------------------------------
% File     : PUZ031+3 : TPTP v5.2.0. Released v4.1.0.
% Domain   : Puzzles
% Problem  : Schubert's Steamroller
% Version  : Especial.
% English  : Wolves, foxes, birds, caterpillars, and snails are animals, and
%            there are some of each of them. Also there are some grains, and
%            grains are plants. Every animal either likes to eat all plants
%            or all animals much smaller than itself that like to eat some
%            plants. Caterpillars and snails are much smaller than birds,
%            which are much smaller than foxes, which in turn are much
%            smaller than wolves. Wolves do not like to eat foxes or grains,
%            while birds like to eat caterpillars but not snails.
%            Caterpillars and snails like to eat some plants. Therefore
%            there is an animal that likes to eat a grain eating animal.

% Refs     : [Pel86] Pelletier (1986), Seventy-five Problems for Testing Au
% Source   : [TPTP]
% Names    : 

% Status   : Theorem
% Rating   : 0.13 v5.2.0, 0.07 v5.0.0, 0.00 v4.1.0
% Syntax   : Number of formulae    :   29 (   9 unit)
%            Number of atoms       :   68 (   0 equality)
%            Maximal formula depth :   10 (   4 average)
%            Number of connectives :   42 (   3   ~;   1   |;  16   &)
%                                         (   0 <=>;  22  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&)
%            Number of predicates  :   11 (   0 propositional; 1-2 arity)
%            Number of functors    :    0 (   0 constant; --- arity)
%            Number of variables   :   44 (   0 sgn;  29   !;  15   ?)
%            Maximal term depth    :    1 (   1 average)
% SPC      : FOF_THM_RFO_NEQ

% Comments : 
%------------------------------------------------------------------------------
fof(edible_type,axiom,(
    ? [A] : edible(A) )).

fof(animal_type,axiom,(
    ? [A] : animal(A) )).

fof(animal_is_edible,axiom,(
    ! [A] :
      ( animal(A)
     => edible(A) ) )).

fof(wolf_type,axiom,(
    ? [A] : wolf(A) )).

fof(wolf_is_animal,axiom,(
    ! [A] :
      ( wolf(A)
     => animal(A) ) )).

fof(fox_type,axiom,(
    ? [A] : fox(A) )).

fof(fox_is_animal,axiom,(
    ! [A] :
      ( fox(A)
     => animal(A) ) )).

fof(bird_type,axiom,(
    ? [A] : bird(A) )).

fof(bird_is_animal,axiom,(
    ! [A] :
      ( bird(A)
     => animal(A) ) )).

fof(caterpillar_type,axiom,(
    ? [A] : caterpillar(A) )).

fof(caterpillar_is_animal,axiom,(
    ! [A] :
      ( caterpillar(A)
     => animal(A) ) )).

fof(snail_type,axiom,(
    ? [A] : snail(A) )).

fof(snail_is_animal,axiom,(
    ! [A] :
      ( snail(A)
     => animal(A) ) )).

fof(plant_type,axiom,(
    ? [A] : plant(A) )).

fof(plant_is_edible,axiom,(
    ! [A] :
      ( plant(A)
     => edible(A) ) )).

fof(grain_type,axiom,(
    ? [A] : grain(A) )).

fof(grain_is_plant,axiom,(
    ! [A] :
      ( grain(A)
     => plant(A) ) )).

fof(pel47_7,axiom,(
    ! [X] :
      ( animal(X)
     => ( ! [Y] :
            ( plant(Y)
           => eats(X,Y) )
        | ! [Y1] :
            ( animal(Y1)
           => ( ( much_smaller(Y1,X)
                & ? [Z] :
                    ( plant(Z)
                    & eats(Y1,Z) ) )
             => eats(X,Y1) ) ) ) ) )).

fof(pel47_8,axiom,(
    ! [Y,X] :
      ( ( bird(Y)
        & snail(X) )
     => much_smaller(X,Y) ) )).

fof(pel47_8a,axiom,(
    ! [Y,X] :
      ( ( bird(Y)
        & caterpillar(X) )
     => much_smaller(X,Y) ) )).

fof(pel47_9,axiom,(
    ! [X,Y] :
      ( ( bird(X)
        & fox(Y) )
     => much_smaller(X,Y) ) )).

fof(pel47_10,axiom,(
    ! [X,Y] :
      ( ( fox(X)
        & wolf(Y) )
     => much_smaller(X,Y) ) )).

fof(pel47_11,axiom,(
    ! [X,Y] :
      ( ( wolf(X)
        & fox(Y) )
     => ~ eats(X,Y) ) )).

fof(pel47_11a,axiom,(
    ! [X,Y] :
      ( ( wolf(X)
        & grain(Y) )
     => ~ eats(X,Y) ) )).

fof(pel47_12,axiom,(
    ! [X,Y] :
      ( ( bird(X)
        & caterpillar(Y) )
     => eats(X,Y) ) )).

fof(pel47_13,axiom,(
    ! [X,Y] :
      ( ( bird(X)
        & snail(Y) )
     => ~ eats(X,Y) ) )).

fof(pel47_14,axiom,(
    ! [X] :
      ( caterpillar(X)
     => ? [Y] :
          ( plant(Y)
          & eats(X,Y) ) ) )).

fof(pel47_14a,axiom,(
    ! [X] :
      ( snail(X)
     => ? [Y] :
          ( plant(Y)
          & eats(X,Y) ) ) )).

fof(pel47,conjecture,(
    ? [X,Y,Z] :
      ( animal(X)
      & animal(Y)
      & grain(Z)
      & eats(Y,Z)
      & eats(X,Y) ) )).

%------------------------------------------------------------------------------
