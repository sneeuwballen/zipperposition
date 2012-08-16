%------------------------------------------------------------------------------
% File     : PUZ122^5 : TPTP v5.2.0. Bugfixed v5.2.0.
% Domain   : Puzzles
% Problem  : TPS problem from CHECKERBOARD-THMS
% Version  : Especial.
% English  :

% Refs     : [Bro09] Brown (2009), Email to Geoff Sutcliffe
% Source   : [Bro09]
% Names    : tps_0816 [Bro09]

% Status   : Unknown
% Rating   : 1.00 v5.2.0
% Syntax   : Number of formulae    :    9 (   0 unit;   4 type;   4 defn)
%            Number of atoms       :   90 (  10 equality;  46 variable)
%            Maximal formula depth :   17 (   9 average)
%            Number of connectives :   42 (   2   ~;   1   |;   8   &;  28   @)
%                                         (   0 <=>;   3  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&;   0  !!;   0  ??)
%            Number of type conns  :   41 (  41   >;   0   *;   0   +;   0  <<)
%            Number of symbols     :    7 (   4   :)
%            Number of variables   :   25 (   0 sgn;  11   !;   5   ?;   9   ^)
%                                         (  25   :;   0  !>;   0  ?*)
%                                         (   0  @-;   0  @+)
% SPC      : THF_UNK

% Comments : This problem is from the TPS library. Copyright (c) 2009 The TPS
%            project in the Department of Mathematical Sciences at Carnegie
%            Mellon University. Distributed under the Creative Commons copyleft
%            license: http://creativecommons.org/licenses/by-sa/3.0/
%          : THF0 syntax
% Bugfixes : v5.2.0 - Added missing type declarations.
%------------------------------------------------------------------------------
thf(cCKB_FIN_type,type,(
    cCKB_FIN: ( $i > $i > $o ) > $o )).

thf(cCKB_INF_type,type,(
    cCKB_INF: ( $i > $i > $o ) > $o )).

thf(cCKB_INJ_type,type,(
    cCKB_INJ: ( $i > $i > $i > $i > $o ) > $o )).

thf(cCKB_XPL_type,type,(
    cCKB_XPL: ( $i > $i > $i > $i > $o ) > ( $i > $i > $o ) > $i > $i > $o )).

thf(cCKB_FIN_def,definition,
    ( cCKB_FIN
    = ( ^ [Xk: $i > $i > $o] :
          ~ ( cCKB_INF @ Xk ) ) )).

thf(cCKB_INF_def,definition,
    ( cCKB_INF
    = ( ^ [Xk: $i > $i > $o] :
        ? [Xh: $i > $i > $i > $i > $o,Xm: $i,Xn: $i] :
          ( ( cCKB_INJ @ Xh )
          & ( cCKB_XPL @ Xh @ Xk @ Xm @ Xn ) ) ) )).

thf(cCKB_INJ_def,definition,
    ( cCKB_INJ
    = ( ^ [Xh: $i > $i > $i > $i > $o] :
        ! [Xx1: $i,Xy1: $i,Xx2: $i,Xy2: $i,Xu: $i,Xv: $i] :
          ( ( ( Xh @ Xx1 @ Xy1 @ Xu @ Xv )
            & ( Xh @ Xx2 @ Xy2 @ Xu @ Xv ) )
         => ( ( Xx1 = Xx2 )
            & ( Xy1 = Xy2 ) ) ) ) )).

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

thf(cL2000_pme,conjecture,(
    ! [Xk: $i > $i > $o,Xz: $i,Xw: $i] :
      ( ( cCKB_FIN @ Xk )
     => ( cCKB_FIN
        @ ^ [Xu: $i,Xv: $i] :
            ( ( Xk @ Xu @ Xv )
            | ( ( Xu = Xz )
              & ( Xv = Xw ) ) ) ) ) )).

%------------------------------------------------------------------------------
