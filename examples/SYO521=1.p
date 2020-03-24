%------------------------------------------------------------------------------
% File     : SYO521=1 : TPTP v6.1.0. Released v5.0.0.
% Domain   : Syntactic
% Problem  : There are more than two integers
% Version  : Especial.
% English  : 

% Refs     : 
% Source   : [TPTP]
% Names    :

% Status   : CounterSatisfiable
% Rating   : 0.00 v6.0.0, 0.50 v5.4.0, 0.67 v5.2.0, 1.00 v5.0.0
% Syntax   : Number of formulae    :    3 (   2 unit;   2 type)
%            Number of atoms       :    5 (   3 equality)
%            Maximal formula depth :    5 (   3 average)
%            Number of connectives :    2 (   0   ~;   1   |;   1   &)
%                                         (   0 <=>;   0  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&)
%            Number of type conns  :    0 (   0   >;   0   *;   0   +;   0  <<)
%            Number of predicates  :    4 (   3 propositional; 0-2 arity)
%            Number of functors    :    4 (   3 constant; 0-2 arity)
%            Number of variables   :    2 (   0 sgn;   1   !;   1   ?)
%            Maximal term depth    :    2 (   1 average)
%            Arithmetic symbols    :    3 (   1 pred;    1 func;    1 numbers)
% SPC      : TF0_CSA_EQU_ARI

% Comments : Designed to test for soundness
%------------------------------------------------------------------------------
tff(a_type,type,(
    a: $int )).

tff(b_type,type,(
    b: $int )).

tff(a,conjecture,(
    ? [X: $int] :
      ( $sum(2,2) = X
      & ! [Y: $int] :
          ( Y = a
          | Y = b ) ) )).

%------------------------------------------------------------------------------
