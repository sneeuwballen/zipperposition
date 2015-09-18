%------------------------------------------------------------------------------
% File     : ARI340=1 : TPTP v6.1.0. Released v5.0.0.
% Domain   : Arithmetic
% Problem  : Rational: 15/2 is less than sum of 29/10 and 24/5
% Version  : Especial.
% English  :

% Refs     :
% Source   : [TPTP]
% Names    :

% Status   : Theorem
% Rating   : 0.38 v6.1.0, 0.22 v6.0.0, 0.00 v5.5.0, 0.22 v5.4.0, 0.12 v5.3.0, 0.40 v5.2.0, 0.33 v5.1.0, 0.20 v5.0.0
% Syntax   : Number of formulae    :    1 (   0 unit;   0 type)
%            Number of atoms       :    2 (   1 equality)
%            Maximal formula depth :    3 (   3 average)
%            Number of connectives :    1 (   0   ~;   0   |;   0   &)
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
tff(rat_combined_problem_6,conjecture,(
    ! [X: $rat] : 
      ( $sum(29/10,24/5) = X
     => $less(15/2,X) ) )).
%------------------------------------------------------------------------------
