%--------------------------------------------------------------------------
% File     : ARI185=1 : TPTP v6.1.0. Released v5.0.0.
% Domain   : Arithmetic
% Problem  : Integer: Positive function formula
% Version  : Especial.
% English  : 

% Refs     : [PW06]  Prevosto & Waldmann (2006), SPASS+T
%          : [Wal10] Waldmann (2010), Email to Geoff Sutcliffe
% Source   : [Wal10]
% Names    : (20) [PW06]

% Status   : Theorem
% Rating   : 0.20 v6.1.0, 0.33 v6.0.0, 0.38 v5.5.0, 0.50 v5.4.0, 0.62 v5.3.0, 0.71 v5.2.0, 1.00 v5.0.0
% Syntax   : Number of formulae    :    2 (   0 unit;   1 type)
%            Number of atoms       :    4 (   0 equality)
%            Maximal formula depth :    3 (   3 average)
%            Number of connectives :    1 (   0   ~;   0   |;   0   &)
%                                         (   0 <=>;   1  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&)
%            Number of type conns  :    1 (   1   >;   0   *;   0   +;   0  <<)
%            Number of predicates  :    4 (   2 propositional; 0-2 arity)
%            Number of functors    :    8 (   5 constant; 0-2 arity)
%            Number of variables   :    1 (   0 sgn;   1   !;   0   ?)
%            Maximal term depth    :    4 (   2 average)
%            Arithmetic symbols    :   10 (   3 pred;    2 func;    5 numbers)
% SPC      : TF0_THM_NEQ_ARI

% Comments : 
%--------------------------------------------------------------------------
tff(f_type,type,(
    f: $int > $int )).

tff(co1,conjecture,
    ( ! [U: $int] : $greater(f(U),1)
   => $less($difference(7,$product(2,f(3))),4) )).
%--------------------------------------------------------------------------
