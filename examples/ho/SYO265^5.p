%------------------------------------------------------------------------------
% File     : SYO265^5 : TPTP v6.2.0. Released v4.0.0.
% Domain   : Syntactic
% Problem  : TPS problem X5210
% Version  : Especial.
% English  :

% Refs     : [Bro09] Brown (2009), Email to Geoff Sutcliffe
% Source   : [Bro09]
% Names    : tps_0178 [Bro09]
%          : X5210 [TPS]

% Status   : Theorem
% Rating   : 0.14 v5.5.0, 0.17 v5.4.0, 0.20 v4.1.0, 0.00 v4.0.1, 0.33 v4.0.0
% Syntax   : Number of formulae    :    3 (   2 unit;   2 type;   0 defn)
%            Number of atoms       :   13 (   4 equality;   5 variable)
%            Maximal formula depth :    6 (   3 average)
%            Number of connectives :    2 (   0   ~;   0   |;   1   &;   1   @)
%                                         (   0 <=>;   0  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&;   0  !!;   0  ??)
%            Number of type conns  :    0 (   0   >;   0   *;   0   +;   0  <<)
%            Number of symbols     :    3 (   2   :)
%            Number of variables   :    4 (   0 sgn;   0   !;   1   ?;   3   ^)
%                                         (   4   :;   0  !>;   0  ?*)
%                                         (   0  @-;   0  @+)
% SPC      : TH0_THM_EQU

% Comments : This problem is from the TPS library. Copyright (c) 2009 The TPS
%            project in the Department of Mathematical Sciences at Carnegie
%            Mellon University. Distributed under the Creative Commons copyleft
%            license: http://creativecommons.org/licenses/by-sa/3.0/
%          : 
%------------------------------------------------------------------------------
thf(a_type,type,(
    a: $tType )).

thf(x,type,(
    x: a )).

thf(cX5210,conjecture,
    ( ( ^ [Xx: a,Xy: a] : ( Xx = Xy )
      @ x )
    = ( ^ [Xz: a] :
        ? [Xy: a] :
          ( ( Xy = x )
          & ( Xz = Xy ) ) ) )).

%------------------------------------------------------------------------------
