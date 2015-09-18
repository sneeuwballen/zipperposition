%------------------------------------------------------------------------------
% File     : ARI570=1 : TPTP v6.1.0. Released v5.1.0.
% Domain   : Arithmetic
% Problem  : Weakening an inequation
% Version  : Especial.
% English  :

% Refs     : [Wal10] Waldmann (2010), Email to Geoff Sutcliffe
% Source   : [Wal10]
% Names    :

% Status   : Theorem
% Rating   : 0.00 v6.1.0, 0.22 v6.0.0, 0.25 v5.3.0, 0.29 v5.2.0, 0.60 v5.1.0
% Syntax   : Number of formulae    :    1 (   0 unit;   0 type)
%            Number of atoms       :    2 (   0 equality)
%            Maximal formula depth :    4 (   4 average)
%            Number of connectives :    1 (   0   ~;   0   |;   0   &)
%                                         (   0 <=>;   1  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&)
%            Number of type conns  :    0 (   0   >;   0   *;   0   +;   0  <<)
%            Number of predicates  :    2 (   1 propositional; 0-2 arity)
%            Number of functors    :    2 (   1 constant; 0-2 arity)
%            Number of variables   :    2 (   0 sgn;   2   !;   0   ?)
%            Maximal term depth    :    2 (   1 average)
%            Arithmetic symbols    :    4 (   2 pred;    1 func;    1 numbers)
% SPC      : TF0_THM_NEQ_ARI

% Comments :
%------------------------------------------------------------------------------
tff(weakening_ineq,conjecture,(
    ! [X: $int,Y: $int] :
      ( $less(Y,X)
     => $less(Y,$sum(X,3)) ) )).

%------------------------------------------------------------------------------
