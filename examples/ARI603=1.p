%------------------------------------------------------------------------------
% File     : ARI603=1 : TPTP v6.1.0. Released v5.1.0.
% Domain   : Arithmetic
% Problem  : If f(X) > X, then Y = Z + (Y-Z) < Z + f(Y-Z)
% Version  : Especial.
% English  :

% Refs     : [Wal10] Waldmann (2010), Email to Geoff Sutcliffe
% Source   : [Wal10]
% Names    :

% Status   : Theorem
% Rating   : 0.60 v6.1.0, 0.67 v6.0.0, 0.62 v5.5.0, 0.50 v5.4.0, 0.75 v5.3.0, 0.86 v5.2.0, 1.00 v5.1.0
% Syntax   : Number of formulae    :    2 (   0 unit;   1 type)
%            Number of atoms       :    4 (   0 equality)
%            Maximal formula depth :    5 (   4 average)
%            Number of connectives :    1 (   0   ~;   0   |;   0   &)
%                                         (   0 <=>;   1  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&)
%            Number of type conns  :    1 (   1   >;   0   *;   0   +;   0  <<)
%            Number of predicates  :    4 (   2 propositional; 0-2 arity)
%            Number of functors    :    2 (   0 constant; 1-2 arity)
%            Number of variables   :    4 (   0 sgn;   3   !;   1   ?)
%            Maximal term depth    :    3 (   2 average)
%            Arithmetic symbols    :    4 (   3 pred;    1 func;    0 numbers)
% SPC      : TF0_THM_NEQ_ARI

% Comments :
%------------------------------------------------------------------------------
tff(f_type,type,(
    f: $int > $int )).

tff(fX_gt_X_implies_exist_large_fX,conjecture,
    ( ! [X: $int] : $greater(f(X),X)
   => ! [Y: $int,Z: $int] :
      ? [X: $int] : $less(Y,$sum(Z,f(X))) )).

%------------------------------------------------------------------------------
