%------------------------------------------------------------------------------
% File     : PUZ031+2 : TPTP v5.2.0. Released v4.1.0.
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
% Rating   : 0.09 v5.2.0, 0.07 v5.0.0, 0.00 v4.1.0
% Syntax   : Number of formulae    :   24 (   6 unit)
%            Number of atoms       :   61 (   0 equality)
%            Maximal formula depth :   10 (   4 average)
%            Number of connectives :   40 (   3   ~;   1   |;  17   &)
%                                         (   0 <=>;  19  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&)
%            Number of predicates  :   10 (   0 propositional; 1-2 arity)
%            Number of functors    :    0 (   0 constant; --- arity)
%            Number of variables   :   39 (   0 sgn;  27   !;  12   ?)
%            Maximal term depth    :    1 (   1 average)
% SPC      : FOF_THM_RFO_NEQ

% Comments :
%------------------------------------------------------------------------------
fof(wolf_type,axiom,(
    ? [A] : wolf(A) )).

fof(fox_type,axiom,(
    ? [A] : fox(A) )).

fof(bird_type,axiom,(
    ? [A] : bird(A) )).

fof(caterpillar_type,axiom,(
    ? [A] : caterpillar(A) )).

fof(snail_type,axiom,(
    ? [A] : snail(A) )).

fof(pel47_1_1,axiom,(
    ! [X] :
      ( wolf(X)
     => animal(X) ) )).

fof(pel47_2_1,axiom,(
    ! [X] :
      ( fox(X)
     => animal(X) ) )).

fof(pel47_3_1,axiom,(
    ! [X] :
      ( bird(X)
     => animal(X) ) )).

fof(pel47_4_1,axiom,(
    ! [X] :
      ( caterpillar(X)
     => animal(X) ) )).

fof(pel47_4_2,axiom,(
    ! [X] :
      ( snail(X)
     => animal(X) ) )).

fof(grain_type,axiom,(
    ? [A] : grain(A) )).

fof(pel47_6_2,axiom,(
    ! [X] :
      ( grain(X)
     => plant(X) ) )).

fof(pel47_7,axiom,(
    ! [X] :
      ( animal(X)
     => ( ! [Y] :
            ( plant(Y)
           => eats(X,Y) )
        | ! [Y1] :
            ( ( animal(Y1)
              & much_smaller(Y1,X)
              & ? [Z] :
                  ( plant(Z)
                  & eats(Y1,Z) ) )
           => eats(X,Y1) ) ) ) )).

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
    ? [X,Y] :
      ( animal(X)
      & animal(Y)
      & ? [Z] :
          ( grain(Z)
          & eats(Y,Z)
          & eats(X,Y) ) ) )).

%------------------------------------------------------------------------------
