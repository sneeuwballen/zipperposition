%------------------------------------------------------------------------------
% File     : ARI574=1 : TPTP v6.1.0. Released v5.1.0.
% Domain   : Arithmetic
% Problem  : Inequation system has exactly one solution
% Version  : Especial.
% English  :

% Refs     : [Wal10] Waldmann (2010), Email to Geoff Sutcliffe
% Source   : [Wal10]
% Names    :

% Status   : Theorem
% Rating   : 0.25 v6.1.0, 0.33 v6.0.0, 0.29 v5.5.0, 0.22 v5.4.0, 0.38 v5.3.0, 0.30 v5.2.0, 0.67 v5.1.0
% Syntax   : Number of formulae    :    1 (   0 unit;   0 type)
%            Number of atoms       :    5 (   2 equality)
%            Maximal formula depth :    6 (   6 average)
%            Number of connectives :    4 (   0   ~;   0   |;   3   &)
%                                         (   1 <=>;   0  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&)
%            Number of type conns  :    0 (   0   >;   0   *;   0   +;   0  <<)
%            Number of predicates  :    3 (   1 propositional; 0-2 arity)
%            Number of functors    :    7 (   5 constant; 0-2 arity)
%            Number of variables   :    2 (   0 sgn;   2   !;   0   ?)
%            Maximal term depth    :    2 (   1 average)
%            Arithmetic symbols    :    9 (   2 pred;    2 func;    5 numbers)
% SPC      : TF0_THM_EQU_ARI

% Comments :
%------------------------------------------------------------------------------
tff(ineq_sys_has_1_sol,conjecture,(
    ! [X: $int,Y: $int] :
      ( ( $lesseq(7,$sum(X,Y))
        & $lesseq($sum(X,5),$product(2,Y))
        & $lesseq(Y,4) )
    <=> ( X = 3
        & Y = 4 ) ) )).

%------------------------------------------------------------------------------
