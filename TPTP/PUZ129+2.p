%------------------------------------------------------------------------------
% File     : PUZ129+2 : TPTP v5.2.0. Released v4.0.0.
% Domain   : Puzzles
% Problem  : The grocer is not a cyclist
% Version  : Especial.
%            Theorem formulation : Converted from ACE by the APE [FKK08].
% English  : If every honest and industrious person is healthy, and no grocer 
%            is healthy, and every industrious grocer is honest, and every 
%            cyclist is industrious, and every unhealthy cyclist is dishonest,
%            and no healthy person is unhealthy, and no honest person is
%            dishonest, and every grocer is a person, and every cyclist is a
%            person then no grocer is a cyclist.

% Refs     : [FKK08] Fuchs et al. (2008), Attempto Controlled English for K
% Source   : [TPTP]
% Names    : 

% Status   : Theorem
% Rating   : 0.22 v5.2.0, 0.10 v5.0.0, 0.04 v4.1.0, 0.09 v4.0.0
% Syntax   : Number of formulae    :    1 (   0 unit)
%            Number of atoms       :   36 (  10 equality)
%            Maximal formula depth :   14 (  14 average)
%            Number of connectives :   39 (   4   ~;   0   |;  24   &)
%                                         (   0 <=>;  11  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&)
%            Number of predicates  :    5 (   0 propositional; 1-3 arity)
%            Number of functors    :    6 (   6 constant; 0-0 arity)
%            Number of variables   :   20 (   0 sgn;  10   !;  10   ?)
%            Maximal term depth    :    1 (   1 average)
% SPC      : FOF_THM_RFO_SEQ

% Comments : 
%------------------------------------------------------------------------------
fof(prove,conjecture,
    ( ( ! [A] :
          ( ( person(A)
            & property1(A,honest,pos)
            & property1(A,industrious,pos) )
         => ? [B] :
              ( property1(B,healthy,pos)
              & A = B ) )
      & ! [C] :
          ( grocer(C)
         => ~ ? [D] :
                ( property1(D,healthy,pos)
                & C = D ) )
      & ! [E] :
          ( ( grocer(E)
            & property1(E,industrious,pos) )
         => ? [F] :
              ( property1(F,honest,pos)
              & E = F ) )
      & ! [G] :
          ( cyclist(G)
         => ? [H] :
              ( property1(H,industrious,pos)
              & G = H ) )
      & ! [I] :
          ( ( cyclist(I)
            & property1(I,unhealthy,pos) )
         => ? [J] :
              ( property1(J,dishonest,pos)
              & I = J ) )
      & ! [K] :
          ( ( person(K)
            & property1(K,healthy,pos) )
         => ~ ? [L] :
                ( property1(L,unhealthy,pos)
                & K = L ) )
      & ! [M] :
          ( ( person(M)
            & property1(M,honest,pos) )
         => ~ ? [N] :
                ( property1(N,dishonest,pos)
                & M = N ) )
      & ! [O] :
          ( grocer(O)
         => ? [P] :
              ( person(P)
              & O = P ) )
      & ! [Q] :
          ( cyclist(Q)
         => ? [R] :
              ( person(R)
              & Q = R ) ) )
   => ! [S] :
        ( grocer(S)
       => ~ ? [T] :
              ( cyclist(T)
              & S = T ) ) )).

%------------------------------------------------------------------------------
