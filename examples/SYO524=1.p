%--------------------------------------------------------------------------
% File     : SYO524=1 : TPTP v6.1.0. Released v5.0.0.
% Domain   : Arithmetic
% Problem  : Monotone function
% Version  : Especial.
% English  : 

% Refs     : [PW06]  Prevosto & Waldmann (2006), SPASS+T
%          : [Wal10] Waldmann (2010), Email to Geoff Sutcliffe
% Source   : [Wal10]
% Names    : monotone_function [Wal10]

% Status   : Theorem
% Rating   : 0.20 v6.1.0, 0.44 v6.0.0, 0.50 v5.5.0, 0.38 v5.4.0, 0.62 v5.3.0, 0.57 v5.2.0, 0.80 v5.1.0, 0.75 v5.0.0
% Syntax   : Number of formulae    :    2 (   0 unit;   1 type)
%            Number of atoms       :    5 (   0 equality)
%            Maximal formula depth :    4 (   4 average)
%            Number of connectives :    2 (   0   ~;   0   |;   1   &)
%                                         (   0 <=>;   1  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&)
%            Number of type conns  :    1 (   1   >;   0   *;   0   +;   0  <<)
%            Number of predicates  :    3 (   2 propositional; 0-2 arity)
%            Number of functors    :    7 (   5 constant; 0-2 arity)
%            Number of variables   :    1 (   0 sgn;   1   !;   0   ?)
%            Maximal term depth    :    3 (   2 average)
%            Arithmetic symbols    :    8 (   2 pred;    1 func;    5 numbers)
% SPC      : TF0_THM_NEQ_ARI

% Comments : 
%--------------------------------------------------------------------------
tff(f_type,type,(
    f: $int > $int )).

tff(co1,conjecture,
    ( ( ! [U: $int] : $lesseq(f($sum(U,1)),f($sum(U,2)))
      & $lesseq(f(7),3) )
   => $lesseq(f(4),3) )).
%--------------------------------------------------------------------------
