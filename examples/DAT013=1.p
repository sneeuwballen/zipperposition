%------------------------------------------------------------------------------
% File     : DAT013=1 : TPTP v6.2.0. Released v5.0.0.
% Domain   : Data Structures
% Problem  : Compare elements 3
% Version  : [PW06] axioms.
% English  :

% Refs     : [PW06]  Prevosto & Waldmann (2006), SPASS+T
%          : [Wal10] Waldmann (2010), Email to Geoff Sutcliffe
% Source   : [Wal10]
% Names    : (37) [PW06]

% Status   : Theorem
% Rating   : 0.14 v6.2.0, 0.38 v6.1.0, 0.44 v6.0.0, 0.43 v5.5.0, 0.33 v5.4.0, 0.50 v5.3.0, 0.60 v5.2.0, 0.50 v5.1.0, 0.80 v5.0.0
% Syntax   : Number of formulae    :    6 (   2 unit;   3 type)
%            Number of atoms       :   17 (   3 equality)
%            Maximal formula depth :    8 (   5 average)
%            Number of connectives :    6 (   0   ~;   1   |;   2   &)
%                                         (   0 <=>;   3  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&)
%            Number of type conns  :    5 (   2   >;   3   *;   0   +;   0  <<)
%            Number of predicates  :    8 (   5 propositional; 0-2 arity)
%            Number of functors    :    5 (   2 constant; 0-3 arity)
%            Number of variables   :   12 (   0 sgn;  12   !;   0   ?)
%            Maximal term depth    :    3 (   1 average)
%            Arithmetic symbols    :    6 (   3 pred;    1 func;    2 numbers)
% SPC      : TF0_THM_EQU_ARI

% Comments :
%------------------------------------------------------------------------------
%----Includes axioms for arrays
include('Axioms/DAT001=0.ax').
%------------------------------------------------------------------------------
tff(co1,conjecture,(
    ! [U: array,V: $int,W: $int] :
      ( ! [X: $int] :
          ( ( $lesseq(V,X)
            & $lesseq(X,W) )
         => $greater(read(U,X),0) )
     => ! [Y: $int] :
          ( ( $lesseq($sum(V,3),Y)
            & $lesseq(Y,W) )
         => $greater(read(U,Y),0) ) ) )).
%------------------------------------------------------------------------------
