%------------------------------------------------------------------------------
% File     : ARI163=1 : TPTP v6.1.0. Released v5.0.0.
% Domain   : Arithmetic
% Problem  : Integer: Between -1 and 1 must be 0
% Version  : Especial.
% English  :

% Refs     :
% Source   : [TPTP]
% Names    :

% Status   : Theorem
% Rating   : 0.50 v6.1.0, 0.44 v6.0.0, 0.43 v5.5.0, 0.44 v5.4.0, 0.50 v5.3.0, 0.60 v5.2.0, 0.83 v5.1.0, 0.80 v5.0.0
% Syntax   : Number of formulae    :    1 (   0 unit;   0 type)
%            Number of atoms       :    3 (   1 equality)
%            Maximal formula depth :    4 (   4 average)
%            Number of connectives :    2 (   0   ~;   0   |;   1   &)
%                                         (   0 <=>;   1  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&)
%            Number of type conns  :    0 (   0   >;   0   *;   0   +;   0  <<)
%            Number of predicates  :    3 (   1 propositional; 0-2 arity)
%            Number of functors    :    4 (   3 constant; 0-2 arity)
%            Number of variables   :    1 (   0 sgn;   1   !;   0   ?)
%            Maximal term depth    :    2 (   1 average)
%            Arithmetic symbols    :    6 (   2 pred;    1 func;    3 numbers)
% SPC      : TF0_THM_EQU_ARI

% Comments :
%------------------------------------------------------------------------------
tff(sum_something_0_samething,conjecture,(
    ! [X: $int] : 
      ( ( $less(-1,X)
        & $less(X,1) )
     => $sum(21,X) = 21 ) )).
%------------------------------------------------------------------------------
