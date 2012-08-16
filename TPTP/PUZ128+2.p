%------------------------------------------------------------------------------
% File     : PUZ128+2 : TPTP v5.2.0. Released v4.0.0.
% Domain   : Puzzles
% Problem  : Iokaste patricide triangle
% Version  : Especial.
%            Theorem formulation : Converted from ACE by the APE [FKK08].
% English  : Iokaste is a parent of Oedipus. Iokaste is a parent of Polyneikes.
%            Oedipus is a parent of Polyneikes. Polyneikes is a parent of 
%            Thersandros. Oedipus is a patricide. Thersandros is not a 
%            patricide. Therefore, Iokaste is a parent of a patricide that is 
%            a parent of somebody who is not a patricide.

% Refs     : [FKK08] Fuchs et al. (2008), Attempto Controlled English for K
% Source   : [TPTP]
% Names    : 

% Status   : Theorem
% Rating   : 0.41 v5.2.0, 0.20 v5.1.0, 0.19 v5.0.0, 0.29 v4.1.0, 0.39 v4.0.1, 0.43 v4.0.0
% Syntax   : Number of formulae    :    2 (   0 unit)
%            Number of atoms       :   26 (   9 equality)
%            Maximal formula depth :   23 (  18 average)
%            Number of connectives :   26 (   2   ~;   0   |;  24   &)
%                                         (   0 <=>;   0  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&)
%            Number of predicates  :    5 (   1 propositional; 0-3 arity)
%            Number of functors    :    5 (   5 constant; 0-0 arity)
%            Number of variables   :   11 (   0 sgn;   0   !;  11   ?)
%            Maximal term depth    :    1 (   1 average)
% SPC      : FOF_THM_RFO_SEQ

% Comments : 
%------------------------------------------------------------------------------
fof(background,axiom,(
    ? [A,B,C,D,E] :
      ( parent(A)
      & relation(A,of,'Oedipus')
      & 'Iokaste' = A
      & parent(B)
      & relation(B,of,'Polyneikes')
      & 'Iokaste' = B
      & parent(C)
      & relation(C,of,'Polyneikes')
      & 'Oedipus' = C
      & parent(D)
      & relation(D,of,'Thersandros')
      & 'Polyneikes' = D
      & patricide(E)
      & 'Oedipus' = E
      & ~ ? [F] :
            ( patricide(F)
            & 'Thersandros' = F ) ) )).

fof(prove,conjecture,(
    ? [A,B,C,D] :
      ( parent(A)
      & patricide(B)
      & parent(C)
      & $true
      & ~ ? [E] :
            ( patricide(E)
            & D = E )
      & relation(C,of,D)
      & B = C
      & relation(A,of,B)
      & 'Iokaste' = A ) )).

%------------------------------------------------------------------------------
