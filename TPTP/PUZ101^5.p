%------------------------------------------------------------------------------
% File     : PUZ101^5 : TPTP v5.2.0. Bugfixed v5.2.0.
% Domain   : Puzzles
% Problem  : TPS problem from CHECKERBOARD-THMS
% Version  : Especial.
% English  :

% Refs     : [Bro09] Brown (2009), Email to Geoff Sutcliffe
% Source   : [Bro09]
% Names    : tps_0597 [Bro09]

% Status   : CounterSatisfiable
% Rating   : 0.33 v5.2.0
% Syntax   : Number of formulae    :   13 (   4 unit;   9 type;   3 defn)
%            Number of atoms       :  149 (  17 equality;  57 variable)
%            Maximal formula depth :   17 (   7 average)
%            Number of connectives :   84 (   1   ~;   3   |;  15   &;  62   @)
%                                         (   0 <=>;   3  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&;   0  !!;   0  ??)
%            Number of type conns  :   27 (  27   >;   0   *;   0   +;   0  <<)
%            Number of symbols     :   12 (   9   :)
%            Number of variables   :   17 (   0 sgn;   5   !;   2   ?;  10   ^)
%                                         (  17   :;   0  !>;   0  ?*)
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

thf(cCKB_XPL_type,type,(
    cCKB_XPL: ( $i > $i > $i > $i > $o ) > ( $i > $i > $o ) > $i > $i > $o )).

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

thf(cCKB_XPL_def,definition,
    ( cCKB_XPL
    = ( ^ [Xh: $i > $i > $i > $i > $o,Xk: $i > $i > $o,Xm: $i,Xn: $i] :
          ( ( Xk @ Xm @ Xn )
          & ! [Xx: $i,Xy: $i] :
              ( ( Xk @ Xx @ Xy )
             => ? [Xu: $i,Xv: $i] :
                  ( ( Xh @ Xx @ Xy @ Xu @ Xv )
                  & ( Xk @ Xu @ Xv )
                  & ~ ( ( Xu = Xm )
                      & ( Xv = Xn ) ) ) ) ) ) )).

thf(cCKB6_L40000,conjecture,
    ( cCKB_XPL @ cCKB6_H @ cCKB6_BLACK @ c1 @ c1 )).

%------------------------------------------------------------------------------
