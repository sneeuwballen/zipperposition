%------------------------------------------------------------------------------
% File     : PUZ108^5 : TPTP v5.2.0. Bugfixed v5.2.0.
% Domain   : Puzzles
% Problem  : TPS problem from CHECKERBOARD-THMS
% Version  : Especial.
% English  :

% Refs     : [Bro09] Brown (2009), Email to Geoff Sutcliffe
% Source   : [Bro09]
% Names    : tps_0679 [Bro09]

% Status   : Theorem
% Rating   : 0.40 v5.2.0
% Syntax   : Number of formulae    :    4 (   0 unit;   2 type;   1 defn)
%            Number of atoms       :   25 (   1 equality;  12 variable)
%            Maximal formula depth :   12 (   6 average)
%            Number of connectives :   16 (   0   ~;   0   |;   1   &;  12   @)
%                                         (   0 <=>;   3  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&;   0  !!;   0  ??)
%            Number of type conns  :    4 (   4   >;   0   *;   0   +;   0  <<)
%            Number of symbols     :    4 (   2   :)
%            Number of variables   :    6 (   0 sgn;   4   !;   0   ?;   2   ^)
%                                         (   6   :;   0  !>;   0  ?*)
%                                         (   0  @-;   0  @+)
% SPC      : THF_THM_EQU

% Comments : This problem is from the TPS library. Copyright (c) 2009 The TPS
%            project in the Department of Mathematical Sciences at Carnegie
%            Mellon University. Distributed under the Creative Commons copyleft
%            license: http://creativecommons.org/licenses/by-sa/3.0/
%          : THF0 syntax
% Bugfixes : v5.2.0 - Added missing type declarations.
%------------------------------------------------------------------------------
thf(s_type,type,(
    s: $i > $i )).

thf(cCKB_E2_type,type,(
    cCKB_E2: $i > $i > $o )).

thf(cCKB_E2_def,definition,
    ( cCKB_E2
    = ( ^ [Xx: $i,Xy: $i] :
        ! [Xp: $i > $o] :
          ( ( ( Xp @ Xx )
            & ! [Xu: $i] :
                ( ( Xp @ Xu )
               => ( Xp @ ( s @ ( s @ Xu ) ) ) ) )
         => ( Xp @ Xy ) ) ) )).

thf(cCKB_L35000,conjecture,(
    ! [Xx: $i,Xy: $i] :
      ( ( cCKB_E2 @ Xx @ Xy )
     => ( cCKB_E2 @ ( s @ Xx ) @ ( s @ Xy ) ) ) )).

%------------------------------------------------------------------------------
