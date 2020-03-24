%--------------------------------------------------------------------------
% File     : ARI180=1 : TPTP v6.1.0. Released v5.0.0.
% Domain   : Arithmetic
% Problem  : Integer: It must be the function of 0
% Version  : Especial.
% English  : 

% Refs     : [PW06]  Prevosto & Waldmann (2006), SPASS+T
%          : [Wal10] Waldmann (2010), Email to Geoff Sutcliffe
% Source   : [Wal10]
% Names    : (15) [PW06]

% Status   : Theorem
% Rating   : 0.50 v6.1.0, 0.44 v6.0.0, 0.43 v5.5.0, 0.56 v5.4.0, 0.62 v5.3.0, 0.60 v5.2.0, 0.83 v5.1.0, 0.80 v5.0.0
% Syntax   : Number of formulae    :    2 (   0 unit;   1 type)
%            Number of atoms       :    5 (   3 equality)
%            Maximal formula depth :    5 (   4 average)
%            Number of connectives :    2 (   0   ~;   0   |;   1   &)
%                                         (   0 <=>;   1  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&)
%            Number of type conns  :    1 (   1   >;   0   *;   0   +;   0  <<)
%            Number of predicates  :    3 (   2 propositional; 0-2 arity)
%            Number of functors    :    4 (   1 constant; 0-2 arity)
%            Number of variables   :    2 (   0 sgn;   2   !;   0   ?)
%            Maximal term depth    :    3 (   2 average)
%            Arithmetic symbols    :    4 (   1 pred;    2 func;    1 numbers)
% SPC      : TF0_THM_EQU_ARI

% Comments : 
%--------------------------------------------------------------------------
tff(f_type,type,(
    f: $int > $int )).

tff(co1,conjecture,(
    ! [U: $int,V: $int] :
      ( ( $sum(U,V) = f(U)
        & $difference(V,f(U)) = 0 )
     => V = f(0) ) )).
%--------------------------------------------------------------------------
