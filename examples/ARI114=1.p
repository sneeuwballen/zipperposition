%------------------------------------------------------------------------------
% File     : ARI114=1 : TPTP v5.4.0. Released v5.0.0.
% Domain   : Arithmetic
% Problem  : Integer: Product of something and -5 is -10
% Version  : Especial.
% English  :

% Refs     :
% Source   : [TPTP]
% Names    :

% Status   : Theorem
% Rating   : 0.33 v5.4.0, 0.38 v5.3.0, 0.40 v5.2.0, 0.33 v5.1.0, 0.20 v5.0.0
% Syntax   : Number of formulae    :    1 (   1 unit;   0 type)
%            Number of atoms       :    1 (   1 equality)
%            Maximal formula depth :    2 (   2 average)
%            Number of connectives :    0 (   0   ~;   0   |;   0   &)
%                                         (   0 <=>;   0  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&)
%            Number of type conns  :    0 (   0   >;   0   *;   0   +;   0  <<)
%            Number of predicates  :    2 (   1 propositional; 0-2 arity)
%            Number of functors    :    3 (   2 constant; 0-2 arity)
%            Number of variables   :    1 (   0 sgn;   0   !;   1   ?)
%            Maximal term depth    :    2 (   2 average)
%            Arithmetic symbols    :    4 (   1 pred;    1 func;    2 numbers)
% SPC      : TFF_THM_EQU_ARI

% Comments :
%------------------------------------------------------------------------------
tff(product_what_n5_n10,conjecture,(
    ? [X: $int] : $product(X,-5) = -10 )).
%------------------------------------------------------------------------------
