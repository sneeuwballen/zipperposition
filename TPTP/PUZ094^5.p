%------------------------------------------------------------------------------
% File     : PUZ094^5 : TPTP v5.2.0. Released v4.0.0.
% Domain   : Puzzles
% Problem  : TPS problem from BASIC-FO-THMS
% Version  : Especial.
% English  : Reduced version of Schubert's Steamroller.

% Refs     : [Bro09] Brown (2009), Email to Geoff Sutcliffe
% Source   : [Bro09]
% Names    : tps_1213 [Bro09]

% Status   : Theorem
% Rating   : 0.25 v5.1.0, 0.50 v5.0.0, 0.25 v4.1.0, 0.33 v4.0.1, 0.67 v4.0.0
% Syntax   : Number of formulae    :   16 (   5 unit;  15 type;   0 defn)
%            Number of atoms       :  142 (   0 equality;  58 variable)
%            Maximal formula depth :   31 (   5 average)
%            Number of connectives :  148 (  34   ~;  30   |;  19   &;  65   @)
%                                         (   0 <=>;   0  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&;   0  !!;   0  ??)
%            Number of type conns  :   12 (  12   >;   0   *;   0   +;   0  <<)
%            Number of symbols     :   18 (  15   :)
%            Number of variables   :   26 (   0 sgn;  26   !;   0   ?;   0   ^)
%                                         (  26   :;   0  !>;   0  ?*)
%                                         (   0  @-;   0  @+)
% SPC      : THF_THM_NEQ

% Comments : This problem is from the TPS library. Copyright (c) 2009 The TPS
%            project in the Department of Mathematical Sciences at Carnegie
%            Mellon University. Distributed under the Creative Commons copyleft
%            license: http://creativecommons.org/licenses/by-sa/3.0/
%          : THF0 syntax
%------------------------------------------------------------------------------
thf(eats,type,(
    eats: $i > $i > $o )).

thf(grain,type,(
    grain: $i > $o )).

thf(animal,type,(
    animal: $i > $o )).

thf(snail,type,(
    snail: $i > $o )).

thf(sf,type,(
    sf: $i > $i )).

thf(plant,type,(
    plant: $i > $o )).

thf(bird,type,(
    bird: $i > $o )).

thf(wolf,type,(
    wolf: $i > $o )).

thf(fox,type,(
    fox: $i > $o )).

thf(msmaller,type,(
    msmaller: $i > $i > $o )).

thf(a_grain,type,(
    a_grain: $i )).

thf(a_snail,type,(
    a_snail: $i )).

thf(a_bird,type,(
    a_bird: $i )).

thf(a_fox,type,(
    a_fox: $i )).

thf(a_wolf,type,(
    a_wolf: $i )).

thf(cPUZ031_2,conjecture,(
    ~ ( ! [X: $i] :
          ( ( animal @ X )
          | ~ ( wolf @ X ) )
      & ! [X: $i] :
          ( ( animal @ X )
          | ~ ( fox @ X ) )
      & ! [X: $i] :
          ( ( animal @ X )
          | ~ ( bird @ X ) )
      & ! [X: $i] :
          ( ( animal @ X )
          | ~ ( snail @ X ) )
      & ( wolf @ a_wolf )
      & ( fox @ a_fox )
      & ( bird @ a_bird )
      & ( snail @ a_snail )
      & ( grain @ a_grain )
      & ! [X: $i] :
          ( ( plant @ X )
          | ~ ( grain @ X ) )
      & ! [A: $i,P: $i,S: $i,O: $i] :
          ( ( eats @ A @ P )
          | ( eats @ A @ S )
          | ~ ( animal @ A )
          | ~ ( plant @ P )
          | ~ ( animal @ S )
          | ~ ( plant @ O )
          | ~ ( msmaller @ S @ A )
          | ~ ( eats @ S @ O ) )
      & ! [S: $i,B: $i] :
          ( ( msmaller @ S @ B )
          | ~ ( snail @ S )
          | ~ ( bird @ B ) )
      & ! [B: $i,F: $i] :
          ( ( msmaller @ B @ F )
          | ~ ( bird @ B )
          | ~ ( fox @ F ) )
      & ! [F: $i,W: $i] :
          ( ( msmaller @ F @ W )
          | ~ ( fox @ F )
          | ~ ( wolf @ W ) )
      & ! [F: $i,W: $i] :
          ( ~ ( wolf @ W )
          | ~ ( fox @ F )
          | ~ ( eats @ W @ F ) )
      & ! [W: $i,G: $i] :
          ( ~ ( wolf @ W )
          | ~ ( grain @ G )
          | ~ ( eats @ W @ G ) )
      & ! [B: $i,S: $i] :
          ( ~ ( bird @ B )
          | ~ ( snail @ S )
          | ~ ( eats @ B @ S ) )
      & ! [S: $i] :
          ( ( plant @ ( sf @ S ) )
          | ~ ( snail @ S ) )
      & ! [S: $i] :
          ( ( eats @ S @ ( sf @ S ) )
          | ~ ( snail @ S ) )
      & ! [A: $i,E: $i,G: $i] :
          ( ~ ( animal @ A )
          | ~ ( animal @ E )
          | ~ ( grain @ G )
          | ~ ( eats @ A @ E )
          | ~ ( eats @ E @ G ) ) ) )).

%------------------------------------------------------------------------------
