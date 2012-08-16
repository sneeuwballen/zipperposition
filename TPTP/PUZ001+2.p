%------------------------------------------------------------------------------
% File     : PUZ001+2 : TPTP v5.2.0. Released v4.0.0.
% Domain   : Puzzles
% Problem  : Dreadbury Mansion
% Version  : Especial.
%            Theorem formulation : Converted from ACE by the APE [FKK08].
% English  : Someone who lives in DreadburyMansion kills AuntAgatha. If 
%            somebody X lives in DreadburyMansion then X is AuntAgatha or X
%            is the Butler or X is Charles. Everyone hates everyone that he
%            kills. Noone is richer than someone that he kills. Charles hates
%            noone who is hated by AuntAgatha. AuntAgatha does not hate the
%            Butler. Everyone that is not the Butler is hated by AuntAgatha.
%            The Butler hates everyone who is not richer than AuntAgatha. The
%            Butler hates everyone who is hated by AuntAgatha. Noone hates
%            everyone. AuntAgatha is not the Butler. Therefore, AuntAgatha
%            kills AuntAgatha.

% Refs     : [FKK08] Fuchs et al. (2008), Attempto Controlled English for K
% Source   : [TPTP]
% Names    : 

% Status   : Theorem
% Rating   : 0.52 v5.2.0, 0.25 v5.1.0, 0.29 v5.0.0, 0.38 v4.1.0, 0.48 v4.0.1, 0.52 v4.0.0
% Syntax   : Number of formulae    :    2 (   1 unit)
%            Number of atoms       :   38 (   7 equality)
%            Maximal formula depth :   23 (  12 average)
%            Number of connectives :   43 (   7   ~;   2   |;  24   &)
%                                         (   0 <=>;  10  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&)
%            Number of predicates  :    6 (   1 propositional; 0-4 arity)
%            Number of functors    :   10 (  10 constant; 0-0 arity)
%            Number of variables   :   29 (   0 sgn;  14   !;  15   ?)
%            Maximal term depth    :    1 (   1 average)
% SPC      : FOF_THM_RFO_SEQ

% Comments : 
%------------------------------------------------------------------------------
fof(background,axiom,(
    ? [A,B,C] :
      ( $true
      & predicate1(B,live,A)
      & modifier_pp(B,in,'DreadburyMansion')
      & predicate2(C,kill,A,'AuntAgatha')
      & ! [D,E] :
          ( ( $true
            & predicate1(E,live,D)
            & modifier_pp(E,in,'DreadburyMansion') )
         => ( D = 'AuntAgatha'
            | D = 'Butler'
            | D = 'Charles' ) )
      & ! [F] :
          ( $true
         => ! [G,H] :
              ( ( $true
                & predicate2(H,kill,F,G) )
             => ? [I] : predicate2(I,hate,F,G) ) )
      & ! [J] :
          ( $true
         => ~ ? [K,L,M] :
                ( $true
                & predicate2(L,kill,J,K)
                & property2(M,rich,comp_than,K)
                & J = M ) )
      & ! [N,O] :
          ( ( $true
            & predicate2(O,hate,'AuntAgatha',N) )
         => ~ ? [P] : predicate2(P,hate,'Charles',N) )
      & ~ ? [Q] : predicate2(Q,hate,'AuntAgatha','Butler')
      & ! [R] :
          ( ( $true
            & R != 'Butler' )
         => ? [S] : predicate2(S,hate,'AuntAgatha',R) )
      & ! [T] :
          ( ( $true
            & ~ ? [U] :
                  ( property2(U,rich,comp_than,'AuntAgatha')
                  & T = U ) )
         => ? [V] : predicate2(V,hate,'Butler',T) )
      & ! [W,X] :
          ( ( $true
            & predicate2(X,hate,'AuntAgatha',W) )
         => ? [Y] : predicate2(Y,hate,'Butler',W) )
      & ! [Z] :
          ( $true
         => ~ ! [A1] :
                ( $true
               => ? [B1] : predicate2(B1,hate,Z,A1) ) )
      & 'AuntAgatha' != 'Butler' ) )).

fof(prove,conjecture,(
    ? [A] : predicate2(A,kill,'AuntAgatha','AuntAgatha') )).

%------------------------------------------------------------------------------
