%--------------------------------------------------------------------------
% File     : PUZ031-1 : TPTP v5.2.0. Released v1.0.0.
% Domain   : Puzzles
% Problem  : Schubert's Steamroller
% Version  : Especial.
% English  : Wolves, foxes, birds, caterpillars, and snails are animals, and
%            there are some of each of them. Also there are some grains, and
%            grains are plants. Every animal either likes to eat all plants
%            or all animals much smaller than itself that like to eat some
%            plants. Caterpillars and snails are much smaller than birds,
%            which are much smaller than foxes, which in turn are much
%            smaller than wolves. Wolves do not like to eat foxes or grains,
%            while birds like to eat caterpillars but not snails.
%            Caterpillars and snails like to eat some plants. Therefore
%            there is an animal that likes to eat a grain eating animal.

% Refs     : [Sti86] Stickel (1986), Schubert's Steamroller Problem: Formul
%          : [Pel86] Pelletier (1986), Seventy-five Problems for Testing Au
%          : [WB87]  Wang & Bledsoe (1987), Hierarchical Deduction
%          : [MB88]  Manthey & Bry (1988), SATCHMO: A Theorem Prover Implem
% Source   : [Pel86]
% Names    : Pelletier 47 [Pel86]
%          : steamroller.ver1.in [ANL]
%          : steam.in [OTTER]
%          : SST [WB87]

% Status   : Unsatisfiable
% Rating   : 0.00 v2.2.1, 0.25 v2.1.0, 0.00 v2.0.0
% Syntax   : Number of clauses     :   26 (   1 non-Horn;   6 unit;  26 RR)
%            Number of atoms       :   63 (   0 equality)
%            Maximal clause size   :    8 (   2 average)
%            Number of predicates  :   10 (   0 propositional; 1-2 arity)
%            Number of functors    :    8 (   6 constant; 0-1 arity)
%            Number of variables   :   33 (   0 singleton)
%            Maximal term depth    :    2 (   1 average)
% SPC      : CNF_UNS_RFO_NEQ_NHN

% Comments : This problem is named after Len Schubert.
%--------------------------------------------------------------------------
cnf(wolf_is_an_animal,axiom,
    ( animal(X)
    | ~ wolf(X) )).

cnf(fox_is_an_animal,axiom,
    ( animal(X)
    | ~ fox(X) )).

cnf(bird_is_an_animal,axiom,
    ( animal(X)
    | ~ bird(X) )).

cnf(caterpillar_is_an_animal,axiom,
    ( animal(X)
    | ~ caterpillar(X) )).

cnf(snail_is_an_animal,axiom,
    ( animal(X)
    | ~ snail(X) )).

cnf(there_is_a_wolf,axiom,
    ( wolf(a_wolf) )).

cnf(there_is_a_fox,axiom,
    ( fox(a_fox) )).

cnf(there_is_a_bird,axiom,
    ( bird(a_bird) )).

cnf(there_is_a_caterpillar,axiom,
    ( caterpillar(a_caterpillar) )).

cnf(there_is_a_snail,axiom,
    ( snail(a_snail) )).

cnf(there_is_a_grain,axiom,
    ( grain(a_grain) )).

cnf(grain_is_a_plant,axiom,
    ( plant(X)
    | ~ grain(X) )).

cnf(eating_habits,axiom,
    ( eats(Animal,Plant)
    | eats(Animal,Small_animal)
    | ~ animal(Animal)
    | ~ plant(Plant)
    | ~ animal(Small_animal)
    | ~ plant(Other_plant)
    | ~ much_smaller(Small_animal,Animal)
    | ~ eats(Small_animal,Other_plant) )).

cnf(caterpillar_smaller_than_bird,axiom,
    ( much_smaller(Catapillar,Bird)
    | ~ caterpillar(Catapillar)
    | ~ bird(Bird) )).

cnf(snail_smaller_than_bird,axiom,
    ( much_smaller(Snail,Bird)
    | ~ snail(Snail)
    | ~ bird(Bird) )).

cnf(bird_smaller_than_fox,axiom,
    ( much_smaller(Bird,Fox)
    | ~ bird(Bird)
    | ~ fox(Fox) )).

cnf(fox_smaller_than_wolf,axiom,
    ( much_smaller(Fox,Wolf)
    | ~ fox(Fox)
    | ~ wolf(Wolf) )).

cnf(wolf_dont_eat_fox,axiom,
    ( ~ wolf(Wolf)
    | ~ fox(Fox)
    | ~ eats(Wolf,Fox) )).

cnf(wolf_dont_eat_grain,axiom,
    ( ~ wolf(Wolf)
    | ~ grain(Grain)
    | ~ eats(Wolf,Grain) )).

cnf(bird_eats_caterpillar,axiom,
    ( eats(Bird,Catapillar)
    | ~ bird(Bird)
    | ~ caterpillar(Catapillar) )).

cnf(bird_dont_eat_snail,axiom,
    ( ~ bird(Bird)
    | ~ snail(Snail)
    | ~ eats(Bird,Snail) )).

cnf(caterpillar_food_is_a_plant,axiom,
    ( plant(caterpillar_food_of(Catapillar))
    | ~ caterpillar(Catapillar) )).

cnf(caterpillar_eats_caterpillar_food,axiom,
    ( eats(Catapillar,caterpillar_food_of(Catapillar))
    | ~ caterpillar(Catapillar) )).

cnf(snail_food_is_a_plant,axiom,
    ( plant(snail_food_of(Snail))
    | ~ snail(Snail) )).

cnf(snail_eats_snail_food,axiom,
    ( eats(Snail,snail_food_of(Snail))
    | ~ snail(Snail) )).

cnf(prove_the_animal_exists,negated_conjecture,
    ( ~ animal(Animal)
    | ~ animal(Grain_eater)
    | ~ grain(Grain)
    | ~ eats(Animal,Grain_eater)
    | ~ eats(Grain_eater,Grain) )).

%--------------------------------------------------------------------------
