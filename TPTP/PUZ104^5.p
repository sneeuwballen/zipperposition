%------------------------------------------------------------------------------
% File     : PUZ104^5 : TPTP v5.2.0. Bugfixed v5.2.0.
% Domain   : Puzzles
% Problem  : TPS problem from CHECKERBOARD-THMS
% Version  : Especial.
% English  :

% Refs     : [Bro09] Brown (2009), Email to Geoff Sutcliffe
% Source   : [Bro09]
% Names    : tps_0631 [Bro09]

% Status   : Theorem
% Rating   : 0.20 v5.2.0
% Syntax   : Number of formulae    :    5 (   1 unit;   3 type;   1 defn)
%            Number of atoms       :   22 (   1 equality;   9 variable)
%            Maximal formula depth :   10 (   5 average)
%            Number of connectives :   13 (   0   ~;   0   |;   1   &;   9   @)
%                                         (   0 <=>;   3  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&;   0  !!;   0  ??)
%            Number of type conns  :    3 (   3   >;   0   *;   0   +;   0  <<)
%            Number of symbols     :    5 (   3   :)
%            Number of variables   :    4 (   0 sgn;   3   !;   0   ?;   1   ^)
%                                         (   4   :;   0  !>;   0  ?*)
%                                         (   0  @-;   0  @+)
% SPC      : THF_THM_EQU

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

thf(cCKB6_NUM_type,type,(
    cCKB6_NUM: $i > $o )).

thf(cCKB6_NUM_def,definition,
    ( cCKB6_NUM
    = ( ^ [Xx: $i] :
        ! [Xp: $i > $o] :
          ( ( ( Xp @ c1 )
            & ! [Xw: $i] :
                ( ( Xp @ Xw )
               => ( Xp @ ( s @ Xw ) ) ) )
         => ( Xp @ Xx ) ) ) )).

thf(cCKB6_L4000,conjecture,(
    ! [Xx: $i] :
      ( ( cCKB6_NUM @ Xx )
     => ( cCKB6_NUM @ ( s @ ( s @ Xx ) ) ) ) )).

%------------------------------------------------------------------------------
