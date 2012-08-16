%------------------------------------------------------------------------------
% File     : PUZ126^5 : TPTP v5.2.0. Bugfixed v5.2.0.
% Domain   : Puzzles
% Problem  : TPS problem from CHECKERBOARD-THMS
% Version  : Especial.
% English  :

% Refs     : [Bro09] Brown (2009), Email to Geoff Sutcliffe
% Source   : [Bro09]
% Names    : tps_0890 [Bro09]

% Status   : CounterSatisfiable
% Rating   : 0.33 v5.2.0
% Syntax   : Number of formulae    :   11 (   4 unit;   8 type;   2 defn)
%            Number of atoms       :  139 (  17 equality;  49 variable)
%            Maximal formula depth :   16 (   6 average)
%            Number of connectives :   85 (   0   ~;   3   |;  13   &;  66   @)
%                                         (   0 <=>;   3  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&;   0  !!;   0  ??)
%            Number of type conns  :   11 (  11   >;   0   *;   0   +;   0  <<)
%            Number of symbols     :   10 (   8   :)
%            Number of variables   :   13 (   0 sgn;   7   !;   0   ?;   6   ^)
%                                         (  13   :;   0  !>;   0  ?*)
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

thf(c2_type,type,(
    c2: $i )).

thf(c3_type,type,(
    c3: $i )).

thf(c4_type,type,(
    c4: $i )).

thf(g_type,type,(
    g: $i > $i > $i )).

thf(s_type,type,(
    s: $i > $i )).

thf(cCKB6_BLACK_type,type,(
    cCKB6_BLACK: $i > $i > $o )).

thf(cCKB6_H_type,type,(
    cCKB6_H: $i > $i > $i > $i > $o )).

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

thf(cCKB6_H_def,definition,
    ( cCKB6_H
    = ( ^ [Xx: $i,Xy: $i,Xu: $i,Xv: $i] :
          ( ( cCKB6_BLACK @ Xx @ Xy )
          & ( ( ( ( g @ ( s @ ( s @ Xx ) ) @ ( s @ Xy ) )
                = c1 )
              & ( Xu
                = ( s @ ( s @ ( s @ Xx ) ) ) )
              & ( Xv
                = ( s @ Xy ) ) )
            | ( ( ( g @ ( s @ ( s @ Xx ) ) @ ( s @ Xy ) )
                = c2 )
              & ( Xu
                = ( s @ ( s @ Xx ) ) )
              & ( Xv
                = ( s @ ( s @ Xy ) ) ) )
            | ( ( ( g @ ( s @ ( s @ Xx ) ) @ ( s @ Xy ) )
                = c3 )
              & ( Xu
                = ( s @ Xx ) )
              & ( Xv
                = ( s @ Xy ) ) )
            | ( ( ( g @ ( s @ ( s @ Xx ) ) @ ( s @ Xy ) )
                = c4 )
              & ( Xu
                = ( s @ ( s @ Xx ) ) )
              & ( Xv = Xy ) ) ) ) ) )).

thf(cCKB_L43100,conjecture,(
    ! [Xx: $i,Xy: $i,Xu: $i,Xv: $i] :
      ( ( ( cCKB6_H @ Xx @ Xy @ Xu @ Xv )
        & ( ( g @ Xu @ Xv )
          = c3 ) )
     => ( ( ( s @ ( s @ ( s @ ( s @ ( s @ Xu ) ) ) ) )
          = Xx )
        & ( ( s @ ( s @ ( s @ ( s @ ( s @ ( s @ ( s @ Xv ) ) ) ) ) ) )
          = Xy ) ) ) )).

%------------------------------------------------------------------------------
