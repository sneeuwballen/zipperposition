%------------------------------------------------------------------------------
% File     : ARI505=1 : TPTP v6.1.0. Released v5.0.0.
% Domain   : Arithmetic
% Problem  : Mixed: 17.99 coerced to integer is not 18
% Version  : Especial.
% English  :

% Refs     :
% Source   : [TPTP]
% Names    :

% Status   : Theorem
% Rating   : 0.62 v6.1.0, 0.56 v6.0.0, 0.57 v5.5.0, 0.67 v5.4.0, 0.50 v5.3.0, 0.60 v5.2.0, 0.67 v5.1.0, 0.60 v5.0.0
% Syntax   : Number of formulae    :    1 (   1 unit;   0 type)
%            Number of atoms       :    1 (   1 equality)
%            Maximal formula depth :    2 (   2 average)
%            Number of connectives :    1 (   1   ~;   0   |;   0   &)
%                                         (   0 <=>;   0  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&)
%            Number of type conns  :    0 (   0   >;   0   *;   0   +;   0  <<)
%            Number of predicates  :    1 (   0 propositional; 2-2 arity)
%            Number of functors    :    3 (   2 constant; 0-1 arity)
%            Number of variables   :    0 (   0 sgn;   0   !;   0   ?)
%            Maximal term depth    :    2 (   2 average)
%            Arithmetic symbols    :    3 (   0 pred;    1 func;    2 numbers)
% SPC      : TF0_THM_EQU_ARI

% Comments :
%------------------------------------------------------------------------------
tff(mixed_types_problem_10,conjecture,
    ( $to_int(17.99) != 18 )).
%------------------------------------------------------------------------------
