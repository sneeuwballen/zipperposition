%------------------------------------------------------------------------------
% File     : PUZ112^5 : TPTP v5.2.0. Bugfixed v5.2.0.
% Domain   : Puzzles
% Problem  : TPS problem from CHECKERBOARD-THMS
% Version  : Especial.
% English  :

% Refs     : [Bro09] Brown (2009), Email to Geoff Sutcliffe
% Source   : [Bro09]
% Names    : tps_0685 [Bro09]

% Status   : CounterSatisfiable
% Rating   : 0.00 v5.2.0
% Syntax   : Number of formulae    :    9 (   2 unit;   6 type;   2 defn)
%            Number of atoms       :   75 (  10 equality;  10 variable)
%            Maximal formula depth :   14 (   6 average)
%            Number of connectives :   42 (   0   ~;   6   |;   1   &;  34   @)
%                                         (   0 <=>;   1  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&;   0  !!;   0  ??)
%            Number of type conns  :    4 (   4   >;   0   *;   0   +;   0  <<)
%            Number of symbols     :    8 (   6   :)
%            Number of variables   :    3 (   0 sgn;   1   !;   0   ?;   2   ^)
%                                         (   3   :;   0  !>;   0  ?*)
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

thf(cS_type,type,(
    cS: $i > $i )).

thf(cX_type,type,(
    cX: $i )).

thf(s_type,type,(
    s: $i > $i )).

thf(cCKB_EVEN_type,type,(
    cCKB_EVEN: $i > $o )).

thf(cCKB_ODD_type,type,(
    cCKB_ODD: $i > $o )).

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

thf(cCKB_L9000,conjecture,(
    ! [Xx: $i] :
      ( ( cCKB_ODD @ Xx )
     => ( ( cCKB_ODD @ ( s @ ( s @ Xx ) ) )
        & ( cCKB_EVEN @ ( cS @ cX ) ) ) ) )).

%------------------------------------------------------------------------------
