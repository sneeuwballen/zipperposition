%------------------------------------------------------------------------------
% File     : PUZ099^5 : TPTP v5.2.0. Bugfixed v5.2.0.
% Domain   : Puzzles
% Problem  : TPS problem from CHECKERBOARD-THMS
% Version  : Especial.
% English  :

% Refs     : [Bro09] Brown (2009), Email to Geoff Sutcliffe
% Source   : [Bro09]
% Names    : tps_0591 [Bro09]

% Status   : CounterSatisfiable
% Rating   : 0.33 v5.2.0
% Syntax   : Number of formulae    :    6 (   2 unit;   4 type;   1 defn)
%            Number of atoms       :   22 (   1 equality;   8 variable)
%            Maximal formula depth :   12 (   4 average)
%            Number of connectives :   12 (   1   ~;   0   |;   1   &;   8   @)
%                                         (   0 <=>;   2  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&;   0  !!;   0  ??)
%            Number of type conns  :    4 (   4   >;   0   *;   0   +;   0  <<)
%            Number of symbols     :    7 (   4   :)
%            Number of variables   :    4 (   0 sgn;   2   !;   0   ?;   2   ^)
%                                         (   4   :;   0  !>;   0  ?*)
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

thf(cCKB_L38000,conjecture,(
    ~ ( cCKB_E2 @ c1 @ c2 ) )).

%------------------------------------------------------------------------------
