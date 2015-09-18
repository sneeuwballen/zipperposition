%--------------------------------------------------------------------------
% File     : ARI188=1 : TPTP v6.1.0. Released v5.0.0.
% Domain   : Arithmetic
% Problem  : Integer: Sum of something and 3 is 5 in a predicate
% Version  : Especial.
% English  : 

% Refs     : [PW06]  Prevosto & Waldmann (2006), SPASS+T
%          : [Wal10] Waldmann (2010), Email to Geoff Sutcliffe
% Source   : [Wal10]
% Names    : (24) [PW06]

% Status   : Theorem
% Rating   : 0.40 v6.1.0, 0.56 v6.0.0, 0.62 v5.5.0, 0.50 v5.3.0, 0.57 v5.2.0, 0.60 v5.1.0, 0.50 v5.0.0
% Syntax   : Number of formulae    :    2 (   0 unit;   1 type)
%            Number of atoms       :    4 (   0 equality)
%            Maximal formula depth :    3 (   3 average)
%            Number of connectives :    1 (   0   ~;   0   |;   0   &)
%                                         (   0 <=>;   1  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&)
%            Number of type conns  :    1 (   1   >;   0   *;   0   +;   0  <<)
%            Number of predicates  :    4 (   3 propositional; 0-1 arity)
%            Number of functors    :    3 (   2 constant; 0-2 arity)
%            Number of variables   :    1 (   0 sgn;   1   !;   0   ?)
%            Maximal term depth    :    2 (   2 average)
%            Arithmetic symbols    :    4 (   1 pred;    1 func;    2 numbers)
% SPC      : TF0_THM_NEQ_ARI

% Comments : 
%--------------------------------------------------------------------------
tff(p_type,type,(
    p: $int > $o )).

tff(co1,conjecture,
    ( ! [U: $int] : p($sum(U,3))
   => p(5) )).
%--------------------------------------------------------------------------
