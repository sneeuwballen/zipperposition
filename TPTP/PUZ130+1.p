%------------------------------------------------------------------------------
% File     : PUZ130+1 : TPTP v5.2.0. Released v4.1.0.
% Domain   : Puzzles
% Problem  : Garfield and Odie
% Version  : Especial.
% English  : Garfield is a cat and Odie is a dog. Cats and dogs are pets. John
%            is a human. Every pet has a human owner. Jon owns Garfield and
%            Odie, and they are the only cat and dog he owns. If a dog chases
%            a cat, then the cat's owner hates the dog's owner. Odie has chased
%            Garfield. Therefore, Jon hates himself.

% Refs     : 
% Source   : [TPTP]
% Names    :

% Status   : Theorem
% Rating   : 0.11 v5.2.0, 0.00 v4.1.0
% Syntax   : Number of formulae    :   19 (  11 unit)
%            Number of atoms       :   34 (   3 equality)
%            Maximal formula depth :    5 (   3 average)
%            Number of connectives :   15 (   0   ~;   0   |;   3   &)
%                                         (   1 <=>;  11  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&)
%            Number of predicates  :    8 (   0 propositional; 1-2 arity)
%            Number of functors    :    4 (   3 constant; 0-1 arity)
%            Number of variables   :   17 (   0 sgn;  12   !;   5   ?)
%            Maximal term depth    :    2 (   1 average)
% SPC      : FOF_THM_RFO_SEQ

% Comments :
%------------------------------------------------------------------------------
fof(cat_type,axiom,(
    ? [A] : cat(A) )).

fof(garfield_type,axiom,(
    cat(garfield) )).

fof(dog_type,axiom,(
    ? [A] : dog(A) )).

fof(odie_type,axiom,(
    dog(odie) )).

fof(pet_type,axiom,(
    ? [A] : pet(A) )).

fof(dog_pet_type,axiom,(
    ! [A] :
      ( dog(A)
     => pet(A) ) )).

fof(cat_pet_type,axiom,(
    ! [A] :
      ( cat(A)
     => pet(A) ) )).

fof(human_type,axiom,(
    ? [A] : human(A) )).

fof(jon_type,axiom,(
    human(jon) )).

fof(owner_of_type,axiom,(
    ! [A] :
      ( pet(A)
     => human(owner_of(A)) ) )).

fof(pet_owner_axiom,axiom,(
    ! [X] :
      ( pet(X)
     => ? [Y] :
          ( human(Y)
          & owner(X,Y) ) ) )).

fof(jon_o_owner_axiom,axiom,(
    owner(jon,odie) )).

fof(jon_g_owner_axiom,axiom,(
    owner(jon,garfield) )).

fof(jon_only_g_owner_axiom,axiom,(
    ! [X] :
      ( ! [X] :
          ( cat(X)
         => owner(jon,X) )
     => X = garfield ) )).

fof(jon_only_o_owner_axiom,axiom,(
    ! [X] :
      ( ! [X] :
          ( dog(X)
         => owner(jon,X) )
     => X = odie ) )).

fof(cat_chase_axiom,axiom,(
    ! [X,Y] :
      ( ( cat(X)
        & dog(Y) )
     => ( chased(Y,X)
       => hates(owner_of(X),owner_of(Y)) ) ) )).

fof(owner_def,axiom,(
    ! [X,Y] :
      ( ( human(X)
        & pet(Y) )
     => ( owner(X,Y)
      <=> X = owner_of(Y) ) ) )).

fof(odie_chase_axiom,axiom,(
    chased(odie,garfield) )).

fof(jon_conjecture,conjecture,(
    hates(jon,jon) )).

%------------------------------------------------------------------------------
