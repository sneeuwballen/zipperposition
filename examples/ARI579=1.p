%------------------------------------------------------------------------------
% File     : ARI579=1 : TPTP v6.1.0. Released v5.1.0.
% Domain   : Arithmetic
% Problem  : Inequation system is not solvable over $int (e.g., X = Y = 1/2)
% Version  : Especial.
% English  :

% Refs     : [Wal10] Waldmann (2010), Email to Geoff Sutcliffe
% Source   : [Wal10]
% Names    :

% Status   : CounterSatisfiable
% Rating   : 0.00 v6.0.0, 0.33 v5.2.0, 1.00 v5.1.0
% Syntax   : Number of formulae    :    1 (   0 unit;   0 type)
%            Number of atoms       :    3 (   0 equality)
%            Maximal formula depth :    5 (   5 average)
%            Number of connectives :    2 (   0   ~;   0   |;   2   &)
%                                         (   0 <=>;   0  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&)
%            Number of type conns  :    0 (   0   >;   0   *;   0   +;   0  <<)
%            Number of predicates  :    2 (   1 propositional; 0-2 arity)
%            Number of functors    :    6 (   4 constant; 0-2 arity)
%            Number of variables   :    2 (   0 sgn;   0   !;   2   ?)
%            Maximal term depth    :    3 (   1 average)
%            Arithmetic symbols    :    8 (   2 pred;    2 func;    4 numbers)
% SPC      : TF0_CSA_NEQ_ARI

% Comments : A theorem for $rat and $real.
%------------------------------------------------------------------------------
tff(ineq_sys_rat_solvable,conjecture,(
    ? [X: $int,Y: $int] :
      ( $less(0,X)
      & $less(0,Y)
      & $less($sum($product(3,X),$product(4,Y)),6) ) )).

%------------------------------------------------------------------------------
