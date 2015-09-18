%--------------------------------------------------------------------------
% File     : ARI184=1 : TPTP v6.1.0. Released v5.0.0.
% Domain   : Arithmetic
% Problem  : Integer: Monotonic function formula 2
% Version  : Especial.
% English  : 

% Refs     : [PW06]  Prevosto & Waldmann (2006), SPASS+T
%          : [Wal10] Waldmann (2010), Email to Geoff Sutcliffe
% Source   : [Wal10]
% Names    : (19) [PW06]

% Status   : Theorem
% Rating   : 0.80 v6.1.0, 0.89 v6.0.0, 0.88 v5.4.0, 1.00 v5.3.0, 0.86 v5.2.0, 0.80 v5.1.0, 0.75 v5.0.0
% Syntax   : Number of formulae    :    2 (   0 unit;   1 type)
%            Number of atoms       :    5 (   0 equality)
%            Maximal formula depth :    5 (   4 average)
%            Number of connectives :    2 (   0   ~;   0   |;   0   &)
%                                         (   0 <=>;   2  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&)
%            Number of type conns  :    1 (   1   >;   0   *;   0   +;   0  <<)
%            Number of predicates  :    4 (   2 propositional; 0-2 arity)
%            Number of functors    :    4 (   2 constant; 0-2 arity)
%            Number of variables   :    3 (   0 sgn;   3   !;   0   ?)
%            Maximal term depth    :    4 (   2 average)
%            Arithmetic symbols    :    6 (   3 pred;    1 func;    2 numbers)
% SPC      : TF0_THM_NEQ_ARI

% Comments : 
%--------------------------------------------------------------------------
tff(f_type,type,(
    f: $int > $int )).

tff(co1,conjecture,
    ( ! [U: $int,V: $int] :
        ( $less(U,V)
       => $less(f(U),f(V)) )
   => ! [W: $int] : $greater(f($sum(f(W),2)),$sum(f(f(W)),1)) )).
%--------------------------------------------------------------------------
