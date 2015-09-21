%------------------------------------------------------------------------------
% File     : ARI246=1 : TPTP v6.1.0. Released v5.0.0.
% Domain   : Arithmetic
% Problem  : Rational: Sum of 1/2 and 1/4 is 3/4
% Version  : Especial.
% English  :

% Refs     :
% Source   : [TPTP]
% Names    :

% Status   : Theorem
% Rating   : 0.25 v6.1.0, 0.11 v6.0.0, 0.00 v5.5.0, 0.22 v5.4.0, 0.12 v5.3.0, 0.30 v5.2.0, 0.33 v5.1.0, 0.20 v5.0.0
% Syntax   : Number of formulae    :    1 (   1 unit;   0 type)
%            Number of atoms       :    1 (   1 equality)
%            Maximal formula depth :    1 (   1 average)
%            Number of connectives :    0 (   0   ~;   0   |;   0   &)
%                                         (   0 <=>;   0  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&)
%            Number of type conns  :    0 (   0   >;   0   *;   0   +;   0  <<)
%            Number of predicates  :    1 (   0 propositional; 2-2 arity)
%            Number of functors    :    4 (   3 constant; 0-2 arity)
%            Number of variables   :    0 (   0 sgn;   0   !;   0   ?)
%            Maximal term depth    :    2 (   2 average)
%            Arithmetic symbols    :    4 (   0 pred;    1 func;    3 numbers)
% SPC      : TF0_THM_EQU_ARI

% Comments :
%------------------------------------------------------------------------------
tff(rat_sum_problem_1,conjecture,
    ( $sum(1/2,1/4) = 3/4 )).
%------------------------------------------------------------------------------
