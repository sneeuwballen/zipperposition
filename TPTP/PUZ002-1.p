%--------------------------------------------------------------------------
% File     : PUZ002-1 : TPTP v5.2.0. Released v1.0.0.
% Domain   : Puzzles
% Problem  : The Animals Puzzle
% Version  : Especial.
% English  : 1)  The only animals in this house are cats.
%            2)  Every animal is suitable for a pet, that loves to gaze at
%                the moon.
%            3)  When I detest an animal, I avoid it.
%            4)  No animals are carnivorous, unless they prowl at night.
%            5)  No cat fails to kill mice.
%            6)  No animals ever take to me, except what are in this house.
%            7)  Kangaroos are not suitable for pets.
%            8)  None but carnivora kill mice.
%            9)  I detest animals that do not take to me.
%            10) Animals that prowl at night always love to gaze at the moon.
%            The problem is to prove that "I always avoid a kangaroo".

% Refs     : [Car86] Carroll (1986), Lewis Carroll's Symbolic Logic
% Source   : [ANL]
% Names    : animals.ver1.in [ANL]

% Status   : Unsatisfiable
% Rating   : 0.00 v2.0.0
% Syntax   : Number of clauses     :   12 (   1 non-Horn;   2 unit;  11 RR)
%            Number of atoms       :   22 (   0 equality)
%            Maximal clause size   :    2 (   2 average)
%            Number of predicates  :   11 (   0 propositional; 1-1 arity)
%            Number of functors    :    1 (   1 constant; 0-0 arity)
%            Number of variables   :   10 (   0 singleton)
%            Maximal term depth    :    1 (   1 average)
% SPC      : CNF_UNS_EPR

% Comments :
%--------------------------------------------------------------------------
cnf(only_cats_in_house,axiom,
    ( ~ in_house(Cat)
    | cat(Cat) )).

cnf(gazers_are_suitable_pets,axiom,
    ( ~ gazer(Gazer)
    | suitable_pet(Gazer) )).

cnf(avoid_detested,axiom,
    ( ~ detested(Detested)
    | avoided(Detested) )).

cnf(carnivores_are_prowlers,axiom,
    ( ~ carnivore(Carnivore)
    | prowler(Carnivore) )).

cnf(cats_are_mice_killers,axiom,
    ( ~ cat(Cat)
    | mouse_killer(Cat) )).

cnf(in_house_if_takes_to_me,axiom,
    ( ~ takes_to_me(Taken_animal)
    | in_house(Taken_animal) )).

cnf(kangaroos_are_not_pets,axiom,
    ( ~ kangaroo(Kangaroo)
    | ~ suitable_pet(Kangaroo) )).

cnf(mouse_killers_are_carnivores,axiom,
    ( ~ mouse_killer(Killer)
    | carnivore(Killer) )).

cnf(takes_to_me_or_detested,axiom,
    ( takes_to_me(Animal)
    | detested(Animal) )).

cnf(prowlers_are_gazers,axiom,
    ( ~ prowler(Prowler)
    | gazer(Prowler) )).

cnf(kangaroo_is_a_kangaroo,axiom,
    ( kangaroo(the_kangaroo) )).

cnf(avoid_kangaroo,negated_conjecture,
    ( ~ avoided(the_kangaroo) )).

%--------------------------------------------------------------------------
