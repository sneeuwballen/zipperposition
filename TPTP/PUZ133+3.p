%------------------------------------------------------------------------------
% File     : PUZ133+3 : TPTP v5.2.0. Released v4.1.0.
% Domain   : Puzzles
% Problem  : N queens problem has the variable symmetry property
% Version  : Especial > Augmented & Reduced > Especial.
% English  : 

% Refs     : [Bau09] Baumgartner (2009), Email to G. Sutcliffe
%          : [BS09]  Baumgartner & Slaney (2009), Constraint Modelling: A C
% Source   : [Bau09]
% Names    : n-queens-symmetry-no-hacked-lemma [Bau09]

% Status   : CounterSatisfiable
% Rating   : 1.00 v4.1.0
% Syntax   : Number of formulae    :   11 (   3 unit)
%            Number of atoms       :   35 (  13 equality)
%            Maximal formula depth :    8 (   5 average)
%            Number of connectives :   30 (   6   ~;   0   |;  14   &)
%                                         (   3 <=>;   7  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&)
%            Number of predicates  :    5 (   2 propositional; 0-2 arity)
%            Number of functors    :    8 (   2 constant; 0-2 arity)
%            Number of variables   :   23 (   0 sgn;  23   !;   0   ?)
%            Maximal term depth    :    3 (   2 average)
% SPC      : FOF_THM_RFO_SEQ

% Comments : This version removes an inline lemma and adds an axiom that can
%            prove it.
%------------------------------------------------------------------------------
%----queens_p =
%----         forall (i in 1..n, j in i + 1..n) (
%----                 p[i]     != p[j]
%----         /\      p[i] + i != p[j] + j
%----         /\      p[i] - i != p[j] - j
%----         );
%----... in terms of decision variables named p:
fof(queens_p,axiom,
    ( queens_p
   => ! [I,J] :
        ( ( le(s(n0),I)
          & le(I,n)
          & le(s(I),J)
          & le(J,n) )
       => ( p(I) != p(J)
          & plus(p(I),I) != plus(p(J),J)
          & minus(p(I),I) != minus(p(J),J) ) ) )).

%----The permutation function ('s' is successor):
fof(permutation,axiom,(
    ! [I] : perm(I) = minus(s(n),I) )).

%----... in terms of decision variables named q:
fof(queens_q,axiom,
    ( ! [I,J] :
        ( ( le(s(n0),I)
          & le(I,n)
          & le(s(I),J)
          & le(J,n) )
       => ( q(I) != q(J)
          & plus(q(I),I) != plus(q(J),J)
          & minus(q(I),I) != minus(q(J),J) ) )
   => queens_q )).

%----To prove: "queens_p /\ q is a permutation of p => queens_q"
fof(queens_sym,conjecture,
    ( ( queens_p
      & ! [I] : q(I) = p(perm(I)) )
   => queens_q )).

%----Properties of permutations
%----Permutation stays in range 1..n:
fof(permutation_range,axiom,(
    ! [I] :
      ( ( le(s(n0),I)
        & le(I,n) )
     => ( le(s(n0),perm(I))
        & le(perm(I),n) ) ) )).

%----The permutation is anti-monotone:
fof(permutation_anti_monotone,axiom,(
    ! [I,J] :
      ( lt(I,J)
    <=> lt(perm(J),perm(I)) ) )).

%----Lemma
fof(permutation_another_one,axiom,(
    ! [J,I] : minus(I,J) = minus(perm(J),perm(I)) )).

%----Integer theory axioms
%----Orderings
%----Axioms for less_or_equal:
%----fof(le_ref, axiom,      (! [X] : le(X,X))).
fof(le_trans,axiom,(
    ! [X,Y,Z] :
      ( ( le(X,Y)
        & le(Y,Z) )
     => le(X,Z) ) )).

%----Successors
fof(succ_le,axiom,(
    ! [X] : le(X,s(X)) )).

%----Plus and minus
fof(plus1,axiom,(
    ! [I,J,K,L] :
      ( plus(I,J) = plus(K,L)
    <=> minus(I,K) = minus(L,J) ) )).

%----Important
fof(minus1,axiom,(
    ! [I,J,K,L] :
      ( minus(I,J) = minus(K,L)
    <=> minus(I,K) = minus(J,L) ) )).

%------------------------------------------------------------------------------
