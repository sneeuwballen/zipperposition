%--------------------------------------------------------------------------
% File     : ARI186=1 : TPTP v6.1.0. Released v5.0.0.
% Domain   : Arithmetic
% Problem  : Integer: Function of two arguments
% Version  : Especial.
% English  : 

% Refs     : [PW06]  Prevosto & Waldmann (2006), SPASS+T
%          : [Wal10] Waldmann (2010), Email to Geoff Sutcliffe
% Source   : [Wal10]
% Names    : (22) [PW06]

% Status   : Theorem
% Rating   : 0.50 v6.1.0, 0.56 v6.0.0, 0.57 v5.5.0, 0.56 v5.4.0, 0.62 v5.3.0, 0.60 v5.2.0, 0.83 v5.1.0, 0.80 v5.0.0
% Syntax   : Number of formulae    :    2 (   0 unit;   1 type)
%            Number of atoms       :    6 (   3 equality)
%            Maximal formula depth :    4 (   4 average)
%            Number of connectives :    2 (   0   ~;   0   |;   0   &)
%                                         (   0 <=>;   2  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&)
%            Number of type conns  :    2 (   1   >;   1   *;   0   +;   0  <<)
%            Number of predicates  :    3 (   2 propositional; 0-2 arity)
%            Number of functors    :    6 (   4 constant; 0-2 arity)
%            Number of variables   :    2 (   0 sgn;   2   !;   0   ?)
%            Maximal term depth    :    3 (   2 average)
%            Arithmetic symbols    :    6 (   1 pred;    1 func;    4 numbers)
% SPC      : TF0_THM_EQU_ARI

% Comments : 
%--------------------------------------------------------------------------
tff(g_type,type,(
    g: ( $int * $int ) > $int )).

tff(co1,conjecture,
    ( ! [U: $int,V: $int] : g(U,V) = g(U,$sum(V,2))
   => ( g(3,3) = g(3,4)
     => g(3,2) = g(3,5) ) )).
%--------------------------------------------------------------------------
