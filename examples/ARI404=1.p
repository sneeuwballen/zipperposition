%------------------------------------------------------------------------------
% File     : ARI404=1 : TPTP v6.1.0. Released v5.0.0.
% Domain   : Arithmetic
% Problem  : Real: Sum 3.5 and 2.05 is 5.55
% Version  : Especial.
% English  :

% Refs     :
% Source   : [TPTP]
% Names    :

% Status   : Theorem
% Rating   : 0.12 v6.1.0, 0.11 v6.0.0, 0.00 v5.5.0, 0.22 v5.4.0, 0.12 v5.3.0, 0.30 v5.2.0, 0.50 v5.1.0, 0.40 v5.0.0
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
tff(real_sum_problem_4,conjecture,
    ( $sum(3.5,2.05) = 5.55 )).
%------------------------------------------------------------------------------
