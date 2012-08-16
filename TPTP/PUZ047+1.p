%--------------------------------------------------------------------------
% File     : PUZ047+1 : TPTP v5.2.0. Released v2.5.0.
% Domain   : Syntactic
% Problem  : Taking the wolf, goat, and cabbage across river
% Version  : Especial.
% English  :

% Refs     : [And97] Andrews (1994), Email to G. Sutcliffe
% Source   : [And97]
% Names    : THM100 [And97]

% Status   : Theorem
% Rating   : 0.22 v5.2.0, 0.07 v5.0.0, 0.05 v4.1.0, 0.11 v4.0.0, 0.05 v3.7.0, 0.00 v2.5.0
% Syntax   : Number of formulae    :    1 (   0 unit)
%            Number of atoms       :   30 (   0 equality)
%            Maximal formula depth :   18 (  18 average)
%            Number of connectives :   29 (   0   ~;   0   |;  14   &)
%                                         (   0 <=>;  15  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&)
%            Number of predicates  :    1 (   0 propositional; 5-5 arity)
%            Number of functors    :    7 (   3 constant; 0-1 arity)
%            Number of variables   :   19 (   0 sgn;  18   !;   1   ?)
%            Maximal term depth    :    2 (   1 average)
% SPC      : FOF_THM_RFO_NEQ

% Comments :
%--------------------------------------------------------------------------
fof(thm100,conjecture,
    ( ( p(south,south,south,south,start)
      & ! [T] :
          ( p(south,north,south,north,T)
         => p(north,north,south,north,go_alone(T)) )
      & ! [T1] :
          ( p(north,north,south,north,T1)
         => p(south,north,south,north,go_alone(T1)) )
      & ! [T2] :
          ( p(south,south,north,south,T2)
         => p(north,south,north,south,go_alone(T2)) )
      & ! [T3] :
          ( p(north,south,north,south,T3)
         => p(south,south,north,south,go_alone(T3)) )
      & ! [T4] :
          ( p(south,south,south,north,T4)
         => p(north,north,south,north,take_wolf(T4)) )
      & ! [T5] :
          ( p(north,north,south,north,T5)
         => p(south,south,south,north,take_wolf(T5)) )
      & ! [T6] :
          ( p(south,south,north,south,T6)
         => p(north,north,north,south,take_wolf(T6)) )
      & ! [T7] :
          ( p(north,north,north,south,T7)
         => p(south,south,north,south,take_wolf(T7)) )
      & ! [X,Y,U] :
          ( p(south,X,south,Y,U)
         => p(north,X,north,Y,take_goat(U)) )
      & ! [X1,Y1,V] :
          ( p(north,X1,north,Y1,V)
         => p(south,X1,south,Y1,take_goat(V)) )
      & ! [T8] :
          ( p(south,north,south,south,T8)
         => p(north,north,south,north,take_cabbage(T8)) )
      & ! [T9] :
          ( p(north,north,south,north,T9)
         => p(south,north,south,south,take_cabbage(T9)) )
      & ! [U1] :
          ( p(south,south,north,south,U1)
         => p(north,south,north,north,take_cabbage(U1)) )
      & ! [V1] :
          ( p(north,south,north,north,V1)
         => p(south,south,north,south,take_cabbage(V1)) ) )
   => ? [Z] : p(north,north,north,north,Z) )).

%--------------------------------------------------------------------------
