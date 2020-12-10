%------------------------------------------------------------------------------
% File     : SYO523=1 : TPTP v6.1.0. Released v5.0.0.
% Domain   : Syntactic
% Problem  : Injective pigeon hole function
% Version  : Especial.
% English  : 

% Refs     : [Wal06] Waldmann (2006), Email to Geoff Sutcliffe
% Source   : [Wal06]
% Names    : 

% Status   : Theorem
% Rating   : 0.38 v6.1.0, 0.56 v6.0.0, 0.57 v5.5.0, 0.67 v5.4.0, 0.62 v5.3.0, 0.60 v5.2.0, 0.83 v5.1.0, 0.80 v5.0.0
% Syntax   : Number of formulae    :    2 (   0 unit;   1 type)
%            Number of atoms       :   10 (   2 equality)
%            Maximal formula depth :    6 (   4 average)
%            Number of connectives :    7 (   0   ~;   1   |;   4   &)
%                                         (   0 <=>;   2  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&)
%            Number of type conns  :    1 (   1   >;   0   *;   0   +;   0  <<)
%            Number of predicates  :    5 (   2 propositional; 0-2 arity)
%            Number of functors    :    6 (   5 constant; 0-1 arity)
%            Number of variables   :    2 (   0 sgn;   2   !;   0   ?)
%            Maximal term depth    :    2 (   2 average)
%            Arithmetic symbols    :    8 (   3 pred;    0 func;    5 numbers)
% SPC      : TF0_THM_EQU_ARI

% Comments : 
%------------------------------------------------------------------------------
tff(f_type,type,(
    f: $int > $int )).

tff(injective_f_pigeonhole,conjecture,
    ( ( ! [X: $int,Y: $int] :
          ( f(X) = f(Y)
         => X = Y )
      & $less(6,f(3))
      & $less(f(3),9)
      & $less(6,f(4))
      & $less(f(4),9) )
   => ( $lesseq(f(5),6)
      | $lesseq(9,f(5)) ) )).

%------------------------------------------------------------------------------
