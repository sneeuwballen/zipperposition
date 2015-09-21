%------------------------------------------------------------------------------
% File     : SWV997=1 : TPTP v6.1.0. Released v5.0.0.
% Domain   : Software Verification
% Problem  : Fix-point check 20
% Version  : Especial.
% English  : A problem extracted from model checking a safety problem (no
%            violation of mutual exclusion) for a parameterized system (a
%            variant of the protocol due to Szymanski).

% Refs     : [MP90]  Manna & Pnueli (1990), Tools and Rules for the Practic
%          : [Ran10] Ranise (2010), Email to Geoff Sutcliffe
% Source   : [Ran10]
% Names    : sz2_fixpoint_20 [Ran10]

% Status   : Theorem
% Rating   : 0.12 v6.1.0, 0.44 v6.0.0, 0.57 v5.5.0, 0.44 v5.4.0, 0.50 v5.3.0, 0.30 v5.2.0, 0.50 v5.1.0, 0.20 v5.0.0
% Syntax   : Number of formulae    :    7 (   4 unit;   6 type)
%            Number of atoms       :   62 (  37 equality)
%            Maximal formula depth :   27 (   6 average)
%            Number of connectives :   78 (  25   ~;   0   |;  52   &)
%                                         (   0 <=>;   1  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&)
%            Number of type conns  :    2 (   2   >;   0   *;   0   +;   0  <<)
%            Number of predicates  :   11 (   8 propositional; 0-2 arity)
%            Number of functors    :   15 (  13 constant; 0-1 arity)
%            Number of variables   :   17 (   0 sgn;  17   !;   0   ?)
%            Maximal term depth    :    2 (   1 average)
%            Arithmetic symbols    :   12 (   3 pred;    0 func;    9 numbers)
% SPC      : TF0_THM_EQU_ARI

% Comments : Considered a relatively simple benchmark for infinite state model
%            checkers.
%          : In the SMT AUFLIA logic. Generated using the MCMT system -
%            http://homes.dsi.unimi.it/~ghilardi/mcmt/
%------------------------------------------------------------------------------
tff(z1_type,type,(
    z1: $int )).

tff(z2_type,type,(
    z2: $int )).

tff(z3_type,type,(
    z3: $int )).

tff(z4_type,type,(
    z4: $int )).

tff(a_type,type,(
    a: $int > $int )).

tff(b_type,type,(
    b: $int > $int )).

tff(0,conjecture,
    ( ( ! [Z1: $int] :
          ( $lesseq(1,a(Z1))
          & $lesseq(a(Z1),12) )
      & ! [Z1: $int] :
          ( $lesseq(1,b(Z1))
          & $lesseq(b(Z1),5) )
      & $true
      & z1 != z2
      & z1 != z3
      & z1 != z4
      & z2 != z3
      & z2 != z4
      & z3 != z4
      & ! [Z1: $int,Z2: $int] :
          ~ ( Z1 != Z2
            & a(Z1) = 10
            & a(Z2) = 10 )
      & ! [Z1: $int,Z2: $int] :
          ~ ( Z1 != Z2
            & a(Z1) = 9
            & a(Z2) = 10
            & $less(b(Z2),3)
            & $less(Z2,Z1) )
      & ! [Z1: $int,Z2: $int] :
          ~ ( Z1 != Z2
            & a(Z1) = 8
            & a(Z2) = 10
            & $less(b(Z2),3)
            & $less(Z2,Z1) )
      & ! [Z1: $int,Z2: $int,Z3: $int] :
          ~ ( Z1 != Z2
            & Z1 != Z3
            & Z2 != Z3
            & a(Z1) = 7
            & a(Z2) = 10
            & $less(b(Z2),3)
            & b(Z3) = 5
            & $less(Z2,Z1) )
      & ! [Z1: $int,Z2: $int,Z3: $int] :
          ~ ( Z1 != Z2
            & Z1 != Z3
            & Z2 != Z3
            & a(Z1) = 6
            & a(Z2) = 10
            & $less(b(Z2),3)
            & b(Z3) = 5
            & $less(Z2,Z1) )
      & ! [Z1: $int,Z2: $int,Z3: $int] :
          ~ ( Z1 != Z2
            & Z1 != Z3
            & Z2 != Z3
            & a(Z1) = 7
            & a(Z2) = 10
            & a(Z3) = 8
            & $less(b(Z2),3)
            & $less(Z2,Z1) ) )
   => ~ ( a(z1) = 6
        & a(z2) = 10
        & a(z3) = 1
        & $less(b(z2),3)
        & b(z3) = 5
        & $less(z2,z1) ) )).

%------------------------------------------------------------------------------
