%------------------------------------------------------------------------------
% File     : ARI573=1 : TPTP v6.1.0. Released v5.1.0.
% Domain   : Arithmetic
% Problem  : Three inequations imply a fourth one
% Version  : Especial.
% English  :

% Refs     : [Wal10] Waldmann (2010), Email to Geoff Sutcliffe
% Source   : [Wal10]
% Names    :

% Status   : Theorem
% Rating   : 0.00 v6.1.0, 0.22 v6.0.0, 0.25 v5.5.0, 0.38 v5.4.0, 0.25 v5.3.0, 0.43 v5.2.0, 0.60 v5.1.0
% Syntax   : Number of formulae    :    1 (   0 unit;   0 type)
%            Number of atoms       :    4 (   0 equality)
%            Maximal formula depth :    7 (   7 average)
%            Number of connectives :    3 (   0   ~;   0   |;   2   &)
%                                         (   0 <=>;   1  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&)
%            Number of type conns  :    0 (   0   >;   0   *;   0   +;   0  <<)
%            Number of predicates  :    2 (   1 propositional; 0-2 arity)
%            Number of functors    :    5 (   2 constant; 0-2 arity)
%            Number of variables   :    3 (   0 sgn;   3   !;   0   ?)
%            Maximal term depth    :    3 (   2 average)
%            Arithmetic symbols    :    7 (   2 pred;    3 func;    2 numbers)
% SPC      : TF0_THM_NEQ_ARI

% Comments :
%------------------------------------------------------------------------------
tff(impl_3_ineq,conjecture,(
    ! [X: $int,Y: $int,Z: $int] :
      ( ( $lesseq(1,$sum($product(X,2),$uminus(Y)))
        & $lesseq(1,$sum($product(Y,2),$uminus(Z)))
        & $lesseq(1,$sum($product(Z,2),$uminus(X))) )
     => $lesseq(2,$sum($sum(X,Y),Z)) ) )).

%------------------------------------------------------------------------------
