%------------------------------------------------------------------------------
% File     : PUZ121^5 : TPTP v5.2.0. Bugfixed v5.2.0.
% Domain   : Puzzles
% Problem  : TPS problem from CHECKERBOARD-THMS
% Version  : Especial.
% English  :

% Refs     : [Bro09] Brown (2009), Email to Geoff Sutcliffe
% Source   : [Bro09]
% Names    : tps_0815 [Bro09]

% Status   : CounterSatisfiable
% Rating   : 0.33 v5.2.0
% Syntax   : Number of formulae    :    9 (   1 unit;   5 type;   3 defn)
%            Number of atoms       :   94 (  11 equality;  20 variable)
%            Maximal formula depth :   14 (   7 average)
%            Number of connectives :   58 (   0   ~;   7   |;   4   &;  46   @)
%                                         (   0 <=>;   1  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&;   0  !!;   0  ??)
%            Number of type conns  :    5 (   5   >;   0   *;   0   +;   0  <<)
%            Number of symbols     :    7 (   5   :)
%            Number of variables   :    6 (   0 sgn;   2   !;   0   ?;   4   ^)
%                                         (   6   :;   0  !>;   0  ?*)
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

thf(s_type,type,(
    s: $i > $i )).

thf(cCKB_BLACK_type,type,(
    cCKB_BLACK: $i > $i > $o )).

thf(cCKB_EVEN_type,type,(
    cCKB_EVEN: $i > $o )).

thf(cCKB_ODD_type,type,(
    cCKB_ODD: $i > $o )).

thf(cCKB_BLACK_def,definition,
    ( cCKB_BLACK
    = ( ^ [Xu: $i,Xv: $i] :
          ( ( ( cCKB_ODD @ Xu )
            & ( cCKB_ODD @ Xv ) )
          | ( ( cCKB_EVEN @ Xu )
            & ( cCKB_EVEN @ Xv ) ) ) ) )).

thf(cCKB_EVEN_def,definition,
    ( cCKB_EVEN
    = ( ^ [Xx: $i] :
          ( ( Xx
            = ( s @ c1 ) )
          | ( Xx
            = ( s @ ( s @ ( s @ c1 ) ) ) )
          | ( Xx
            = ( s @ ( s @ ( s @ ( s @ ( s @ c1 ) ) ) ) ) )
          | ( Xx
            = ( s @ ( s @ ( s @ ( s @ ( s @ ( s @ ( s @ c1 ) ) ) ) ) ) ) ) ) ) )).

thf(cCKB_ODD_def,definition,
    ( cCKB_ODD
    = ( ^ [Xx: $i] :
          ( ( Xx = c1 )
          | ( Xx
            = ( s @ ( s @ c1 ) ) )
          | ( Xx
            = ( s @ ( s @ ( s @ ( s @ c1 ) ) ) ) )
          | ( Xx
            = ( s @ ( s @ ( s @ ( s @ ( s @ ( s @ c1 ) ) ) ) ) ) ) ) ) )).

thf(cCKB_L11000,conjecture,(
    ! [Xu: $i,Xv: $i] :
      ( ( cCKB_BLACK @ Xu @ Xv )
     => ( ( cCKB_BLACK @ ( s @ Xu ) @ ( s @ Xv ) )
        & ( cCKB_BLACK @ Xu @ ( s @ ( s @ Xv ) ) )
        & ( cCKB_BLACK @ ( s @ ( s @ Xu ) ) @ Xv ) ) ) )).

%------------------------------------------------------------------------------
