%------------------------------------------------------------------------------
% File     : PUZ123^5 : TPTP v5.2.0. Bugfixed v5.2.0.
% Domain   : Puzzles
% Problem  : TPS problem from CHECKERBOARD-THMS
% Version  : Especial.
% English  :

% Refs     : [Bro09] Brown (2009), Email to Geoff Sutcliffe
% Source   : [Bro09]
% Names    : tps_0846 [Bro09]

% Status   : CounterSatisfiable
% Rating   : 0.33 v5.2.0
% Syntax   : Number of formulae    :    6 (   2 unit;   4 type;   1 defn)
%            Number of atoms       :   49 (   5 equality;  19 variable)
%            Maximal formula depth :   15 (   6 average)
%            Number of connectives :   31 (   1   ~;   1   |;   4   &;  22   @)
%                                         (   0 <=>;   3  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&;   0  !!;   0  ??)
%            Number of type conns  :    5 (   5   >;   0   *;   0   +;   0  <<)
%            Number of symbols     :    7 (   4   :)
%            Number of variables   :    7 (   0 sgn;   5   !;   0   ?;   2   ^)
%                                         (   7   :;   0  !>;   0  ?*)
%                                         (   0  @-;   0  @+)
% SPC      : THF_CSA_EQU

% Comments : This problem is from the TPS library. Copyright (c) 2009 The TPS
%            project in the Department of Mathematical Sciences at Carnegie
%            Mellon University. Distributed under the Creative Commons copyleft
%            license: http://creativecommons.org/licenses/by-sa/3.0/
%          : THF0 syntax
% Bugfixes : v5.2.0 - Added missing type declarations.
%------------------------------------------------------------------------------
thf(c1_type,type,(
    c1: $i )).

thf(c8_type,type,(
    c8: $i )).

thf(s_type,type,(
    s: $i > $i )).

thf(cCKB6_BLACK_type,type,(
    cCKB6_BLACK: $i > $i > $o )).

thf(cCKB6_BLACK_def,definition,
    ( cCKB6_BLACK
    = ( ^ [Xu: $i,Xv: $i] :
        ! [Xw: $i > $i > $o] :
          ( ( ( Xw @ c1 @ c1 )
            & ! [Xj: $i,Xk: $i] :
                ( ( Xw @ Xj @ Xk )
               => ( ( Xw @ ( s @ ( s @ Xj ) ) @ Xk )
                  & ( Xw @ ( s @ Xj ) @ ( s @ Xk ) ) ) ) )
         => ( Xw @ Xu @ Xv ) ) ) )).

thf(cCKB_L39000,conjecture,(
    ! [Xx: $i,Xy: $i] :
      ( ( cCKB6_BLACK @ Xx @ Xy )
     => ~ ( ( ( ( s @ ( s @ Xx ) )
              = c1 )
            & ( ( s @ Xy )
              = c1 ) )
          | ( ( ( s @ ( s @ Xx ) )
              = c8 )
            & ( ( s @ Xy )
              = c8 ) ) ) ) )).

%------------------------------------------------------------------------------
