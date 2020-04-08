%------------------------------------------------------------------------------
% File     : SEV421=1 : TPTP v6.1.0. Released v5.0.0.
% Domain   : Set Theory
% Problem  : Correctness of an efficient emptiness check
% Version  : Especial.
% English  : Using invariant on size to prove correctness of an efficient 
%            emptiness check.

% Refs     : [KNR07] Kuncak et al. (2007), Deciding Boolean Algebra with Pr
%          : [KR07]  Kuncak & Rinard (2007), Towards Efficient Satisfiabili
% Source   : [KR07]
% Names    : VC#1 [KR07]

% Status   : Theorem
% Rating   : 0.00 v6.1.0, 0.11 v6.0.0, 0.14 v5.5.0, 0.11 v5.4.0, 0.12 v5.3.0, 0.30 v5.2.0, 0.17 v5.1.0, 0.20 v5.0.0
% Syntax   : Number of formulae    :   24 (   3 unit;  11 type)
%            Number of atoms       :   56 (  15 equality)
%            Maximal formula depth :    7 (   4 average)
%            Number of connectives :   23 (   4   ~;   1   |;   3   &)
%                                         (  13 <=>;   2  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&)
%            Number of type conns  :   13 (   8   >;   5   *;   0   +;   0  <<)
%            Number of predicates  :   17 (  14 propositional; 0-2 arity)
%            Number of functors    :   10 (   3 constant; 0-2 arity)
%            Number of variables   :   30 (   0 sgn;  30   !;   0   ?)
%            Maximal term depth    :    4 (   1 average)
%            Arithmetic symbols    :    4 (   1 pred;    1 func;    2 numbers)
% SPC      : TF0_THM_EQU_ARI

% Comments : 
%------------------------------------------------------------------------------
tff(set_type,type,(
    set: $tType )).

tff(element_type,type,(
    element: $tType )).

tff(empty_set_type,type,(
    empty_set: set )).

tff(singleton_type,type,(
    singleton: element > set )).

tff(member_type,type,(
    member: ( element * set ) > $o )).

tff(subset_type,type,(
    subset: ( set * set ) > $o )).

tff(intersection_type,type,(
    intersection: ( set * set ) > set )).

tff(union_type,type,(
    union: ( set * set ) > set )).

tff(difference_type,type,(
    difference: ( set * set ) > set )).

tff(complement_type,type,(
    complement: set > set )).

tff(cardinality_type,type,(
    cardinality: set > $int )).

tff(empty_set,axiom,(
    ! [S: set] :
      ( ! [X: element] : ~ member(X,S)
    <=> S = empty_set ) )).

tff(singleton,axiom,(
    ! [X: element,A: element] :
      ( member(X,singleton(A))
    <=> X = A ) )).

tff(subset,axiom,(
    ! [A: set,B: set] :
      ( subset(A,B)
    <=> ! [X: element] :
          ( member(X,A)
         => member(X,B) ) ) )).

tff(intersection,axiom,(
    ! [X: element,A: set,B: set] :
      ( member(X,intersection(A,B))
    <=> ( member(X,A)
        & member(X,B) ) ) )).

tff(union,axiom,(
    ! [X: element,A: set,B: set] :
      ( member(X,union(A,B))
    <=> ( member(X,A)
        | member(X,B) ) ) )).

tff(difference,axiom,(
    ! [B: element,A: set,E: set] :
      ( member(B,difference(E,A))
    <=> ( member(B,E)
        & ~ member(B,A) ) ) )).

tff(complement,axiom,(
    ! [X: element,S: set] :
      ( member(X,S)
    <=> ~ member(X,complement(S)) ) )).

%----From Swen (combined two of his)
tff(cardinality_empty_set,axiom,(
    ! [S: set] :
      ( cardinality(S) = 0
    <=> S = empty_set ) )).

tff(cardinality_intersection_1,axiom,(
    ! [X: element,S: set] :
      ( intersection(singleton(X),S) = singleton(X)
    <=> cardinality(union(singleton(X),S)) = cardinality(S) ) )).

tff(cardinality_intersection_2,axiom,(
    ! [X: element,S: set] :
      ( intersection(singleton(X),S) = empty_set
    <=> cardinality(union(singleton(X),S)) = $sum(cardinality(S),1) ) )).

tff(cardinality_intersection_3,axiom,(
    ! [S: set,T: set] :
      ( cardinality(intersection(S,T)) = 0
    <=> intersection(S,T) = empty_set ) )).

%----From Swen, modified to <=>
tff(cardinality_union,axiom,(
    ! [A: set,B: set] :
      ( intersection(A,B) = empty_set
    <=> cardinality(union(A,B)) = $sum(cardinality(A),cardinality(B)) ) )).

tff(vc1,conjecture,(
    ! [X: element,C: set,Size: $int] :
      ( ( ~ member(X,C)
        & Size = cardinality(C) )
     => ( Size = 0
      <=> C = empty_set ) ) )).
%------------------------------------------------------------------------------
