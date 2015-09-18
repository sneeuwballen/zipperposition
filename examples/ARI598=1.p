%------------------------------------------------------------------------------
% File     : ARI598=1 : TPTP v6.1.0. Released v5.1.0.
% Domain   : Arithmetic
% Problem  : Either a or 3a+1 is even
% Version  : Especial.
% English  :

% Refs     : [Wal10] Waldmann (2010), Email to Geoff Sutcliffe
% Source   : [Wal10]
% Names    :

% Status   : Theorem
% Rating   : 0.80 v6.1.0, 0.89 v6.0.0, 0.88 v5.3.0, 1.00 v5.1.0
% Syntax   : Number of formulae    :    3 (   1 unit;   2 type)
%            Number of atoms       :    6 (   0 equality)
%            Maximal formula depth :    3 (   3 average)
%            Number of connectives :    2 (   0   ~;   0   |;   1   &)
%                                         (   0 <=>;   1  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&)
%            Number of type conns  :    1 (   1   >;   0   *;   0   +;   0  <<)
%            Number of predicates  :    5 (   4 propositional; 0-1 arity)
%            Number of functors    :    6 (   4 constant; 0-2 arity)
%            Number of variables   :    1 (   0 sgn;   0   !;   1   ?)
%            Maximal term depth    :    3 (   2 average)
%            Arithmetic symbols    :    6 (   1 pred;    2 func;    3 numbers)
% SPC      : TF0_THM_NEQ_ARI

% Comments : Also a theorem for $rat and $real, but much easier
%------------------------------------------------------------------------------
tff(p_type,type,(
    p: $int > $o )).

tff(a_type,type,(
    a: $int )).

tff(a_or_3aplus1_even,conjecture,
    ( ( p(a)
      & p($sum($product(3,a),1)) )
   => ? [X: $int] : p($product(2,X)) )).

%------------------------------------------------------------------------------
