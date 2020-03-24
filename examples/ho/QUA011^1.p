%------------------------------------------------------------------------------
% File     : QUA011^1 : TPTP v6.2.0. Released v4.1.0.
% Domain   : Quantales
% Problem  : 0 annihilates arbitrary sums from the right
% Version  : [Hoe09] axioms.
% English  :

% Refs     : [Con71] Conway (1971), Regular Algebra and Finite Machines
%          : [Hoe09] Hoefner (2009), Email to Geoff Sutcliffe
% Source   : [Hoe09]
% Names    : QUA11 [Hoe09] 

% Status   : Theorem
% Rating   : 0.57 v6.0.0, 0.43 v5.5.0, 0.67 v5.4.0, 0.80 v4.1.0
% Syntax   : Number of formulae    :   27 (   2 unit;  12 type;   7 defn)
%            Number of atoms       :  138 (  18 equality;  41 variable)
%            Maximal formula depth :   12 (   5 average)
%            Number of connectives :   46 (   0   ~;   1   |;   4   &;  40   @)
%                                         (   1 <=>;   0  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&;   0  !!;   0  ??)
%            Number of type conns  :   44 (  44   >;   0   *;   0   +;   0  <<)
%            Number of symbols     :   15 (  12   :)
%            Number of variables   :   28 (   1 sgn;   9   !;   4   ?;  15   ^)
%                                         (  28   :;   0  !>;   0  ?*)
%                                         (   0  @-;   0  @+)
% SPC      : TH0_THM_EQU

% Comments : 
%------------------------------------------------------------------------------
%----Include axioms for Quantales
include('Axioms/QUA001^0.ax').
%------------------------------------------------------------------------------
thf(multiplication_anni,conjecture,(
    ! [X: $i > $o] :
      ( ( multiplication @ ( sup @ X ) @ zero )
      = zero ) )).

%------------------------------------------------------------------------------
