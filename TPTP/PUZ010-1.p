%--------------------------------------------------------------------------
% File     : PUZ010-1 : TPTP v5.2.0. Bugfixed v1.0.1.
% Domain   : Puzzle
% Problem  : Who owns the zebra?
% Version  : Especial.
% English  : There are five consecutive houses, each of a different color
%            and inhabited by men of different nationalities. They each
%            own a different pet, have a different favorite drink and
%            drive a different car.
%            1.  The Englishman lives in the red house.
%            2.  The Spaniard owns the dog.
%            3.  Coffee is drunk in the green house.
%            4.  The Ukrainian drinks tea.
%            5.  The green house is immediately to the right of the
%                ivory house.
%            6.  The Porsche driver owns snails.
%            7.  The Masserati is driven by the man who lives in the
%                yellow house.
%            8.  Milk is drunk in the middle house.
%            9.  The Norwegian lives in the first house on the left.
%            10. The man who drives a Saab lives in the house next to
%                the man with the fox.
%            11. The Masserati is driven by the man in the house next
%                to the house where the horse is kept.
%            12. The Honda driver drinks orange juice.
%            13. The Japanese drives a Jaguar.
%            14. The Norwegian lives next to the blue house.
%            The problem is: Who owns the Zebra?  Who drinks water?

% Refs     : [SS86]  Sterling & Shapiro (1986), The Art of Prolog
%          : [LP92]  Lee & Plaisted (1992), Eliminating Duplication with th
%          : [Lee92] Lee (1992), Email to G. Sutcliffe
% Source   : [Lee92]
% Names    : jobs [LP92]

% Status   : Unsatisfiable
% Rating   : 0.00 v3.1.0, 0.11 v2.7.0, 0.17 v2.6.0, 0.44 v2.5.0, 0.50 v2.4.0, 0.75 v2.3.0, 0.67 v2.2.1, 0.00 v2.2.0, 0.75 v2.1.0, 0.50 v2.0.0
% Syntax   : Number of clauses     :  128 (   6 non-Horn;  59 unit; 128 RR)
%            Number of atoms       :  321 (   0 equality)
%            Maximal clause size   :   25 (   3 average)
%            Number of predicates  :   13 (   0 propositional; 1-2 arity)
%            Number of functors    :   30 (  30 constant; 0-0 arity)
%            Number of variables   :   81 (   0 singleton)
%            Maximal term depth    :    1 (   1 average)
% SPC      : CNF_UNS_EPR

% Comments : This is a 'politically correct' version of the original
%            problem, here discussing cars rather than cigarettes.
% Bugfixes : v1.0.1 - Clause ukranian_and_japanese_drink_different, first
%            literal's sign fixed.
%--------------------------------------------------------------------------
%---- Live in one of the houses
cnf(people_live_somewhere,axiom,
    ( ~ person(Person)
    | lives(Person,house_1)
    | lives(Person,house_2)
    | lives(Person,house_3)
    | lives(Person,house_4)
    | lives(Person,house_5) )).

%---- uniqueness.
cnf(english_and_spaniard_live_apart,axiom,
    ( ~ house(H)
    | ~ lives(english,H)
    | ~ lives(spaniard,H) )).

cnf(english_and_norwegian_live_apart,axiom,
    ( ~ house(H)
    | ~ lives(english,H)
    | ~ lives(norwegian,H) )).

cnf(english_and_ukranian_live_apart,axiom,
    ( ~ house(H)
    | ~ lives(english,H)
    | ~ lives(ukranian,H) )).

cnf(english_and_japanese_live_apart,axiom,
    ( ~ house(H)
    | ~ lives(english,H)
    | ~ lives(japanese,H) )).

cnf(spaniard_and_norwegian_live_apart,axiom,
    ( ~ house(H)
    | ~ lives(spaniard,H)
    | ~ lives(norwegian,H) )).

cnf(spaniard_and_ukranian_live_apart,axiom,
    ( ~ house(H)
    | ~ lives(spaniard,H)
    | ~ lives(ukranian,H) )).

cnf(spaniard_and_japanese_live_apart,axiom,
    ( ~ house(H)
    | ~ lives(spaniard,H)
    | ~ lives(japanese,H) )).

cnf(norwegian_and_ukranian_live_apart,axiom,
    ( ~ house(H)
    | ~ lives(norwegian,H)
    | ~ lives(ukranian,H) )).

cnf(norwegian_and_japanese_live_apart,axiom,
    ( ~ house(H)
    | ~ lives(norwegian,H)
    | ~ lives(japanese,H) )).

cnf(ukranian_and_japanese_live_apart,axiom,
    ( ~ house(H)
    | ~ lives(ukranian,H)
    | ~ lives(japanese,H) )).

%---- Drink one of the drinks
cnf(drink_something,axiom,
    ( ~ person(Person)
    | drinks(Person,orange)
    | drinks(Person,coffee)
    | drinks(Person,tea)
    | drinks(Person,milk)
    | drinks(Person,water) )).

%---- uniqueness.
cnf(english_and_spaniard_drink_different,axiom,
    ( ~ drink(H)
    | ~ drinks(english,H)
    | ~ drinks(spaniard,H) )).

cnf(english_and_norwegian_drink_different,axiom,
    ( ~ drink(H)
    | ~ drinks(english,H)
    | ~ drinks(norwegian,H) )).

cnf(english_and_unkranian_drink_different,axiom,
    ( ~ drink(H)
    | ~ drinks(english,H)
    | ~ drinks(ukranian,H) )).

cnf(english_and_japanese_drink_different,axiom,
    ( ~ drink(H)
    | ~ drinks(english,H)
    | ~ drinks(japanese,H) )).

cnf(spaniard_and_norwegian_drink_different,axiom,
    ( ~ drink(H)
    | ~ drinks(spaniard,H)
    | ~ drinks(norwegian,H) )).

cnf(spaniard_and_ukranian_drink_different,axiom,
    ( ~ drink(H)
    | ~ drinks(spaniard,H)
    | ~ drinks(ukranian,H) )).

cnf(spaniard_and_japanese_drink_different,axiom,
    ( ~ drink(H)
    | ~ drinks(spaniard,H)
    | ~ drinks(japanese,H) )).

cnf(norwegian_and_ukranian_drink_different,axiom,
    ( ~ drink(H)
    | ~ drinks(norwegian,H)
    | ~ drinks(ukranian,H) )).

cnf(norwegian_and_japanese_drink_different,axiom,
    ( ~ drink(H)
    | ~ drinks(norwegian,H)
    | ~ drinks(japanese,H) )).

cnf(ukranian_and_japanese_drink_different,axiom,
    ( ~ drink(H)
    | ~ drinks(ukranian,H)
    | ~ drinks(japanese,H) )).

%---- Somke some brand
cnf(drive_something,axiom,
    ( ~ person(Person)
    | drives(Person,masserati)
    | drives(Person,saab)
    | drives(Person,porsche)
    | drives(Person,honda)
    | drives(Person,jaguar) )).

%---- uniqueness.
cnf(english_and_spaniard_drive_different,axiom,
    ( ~ car(H)
    | ~ drives(english,H)
    | ~ drives(spaniard,H) )).

cnf(english_and_norwegian_drive_different,axiom,
    ( ~ car(H)
    | ~ drives(english,H)
    | ~ drives(norwegian,H) )).

cnf(english_and_ukranian_drive_different,axiom,
    ( ~ car(H)
    | ~ drives(english,H)
    | ~ drives(ukranian,H) )).

cnf(english_and_japanese_drive_different,axiom,
    ( ~ car(H)
    | ~ drives(english,H)
    | ~ drives(japanese,H) )).

cnf(spaniard_and_norwegian_drive_different,axiom,
    ( ~ car(H)
    | ~ drives(spaniard,H)
    | ~ drives(norwegian,H) )).

cnf(spaniard_and_ukranian_drive_different,axiom,
    ( ~ car(H)
    | ~ drives(spaniard,H)
    | ~ drives(ukranian,H) )).

cnf(spaniard_and_japanese_drive_different,axiom,
    ( ~ car(H)
    | ~ drives(spaniard,H)
    | ~ drives(japanese,H) )).

cnf(norwegian_and_ukranian_drive_different,axiom,
    ( ~ car(H)
    | ~ drives(norwegian,H)
    | ~ drives(ukranian,H) )).

cnf(norwegian_and_japanese_drive_different,axiom,
    ( ~ car(H)
    | ~ drives(norwegian,H)
    | ~ drives(japanese,H) )).

cnf(ukranian_and_japanese_drive_different,axiom,
    ( ~ car(H)
    | ~ drives(ukranian,H)
    | ~ drives(japanese,H) )).

%---- Own one of the pets
cnf(own_a_pet,axiom,
    ( ~ person(Person)
    | owns(Person,dog)
    | owns(Person,snails)
    | owns(Person,horse)
    | owns(Person,fox)
    | owns(Person,zebra) )).

%---- uniqueness.
cnf(english_and_spaniard_own_different_pets,axiom,
    ( ~ animal(H)
    | ~ owns(english,H)
    | ~ owns(spaniard,H) )).

cnf(english_and_norwegian_own_different_pets,axiom,
    ( ~ animal(H)
    | ~ owns(english,H)
    | ~ owns(norwegian,H) )).

cnf(english_and_ukranian_own_different_pets,axiom,
    ( ~ animal(H)
    | ~ owns(english,H)
    | ~ owns(ukranian,H) )).

cnf(english_and_japanese_own_different_pets,axiom,
    ( ~ animal(H)
    | ~ owns(english,H)
    | ~ owns(japanese,H) )).

cnf(spaniard_and_norwegian_own_different_pets,axiom,
    ( ~ animal(H)
    | ~ owns(spaniard,H)
    | ~ owns(norwegian,H) )).

cnf(spaniard_and_ukranian_own_different_pets,axiom,
    ( ~ animal(H)
    | ~ owns(spaniard,H)
    | ~ owns(ukranian,H) )).

cnf(spaniard_and_japanese_own_different_pets,axiom,
    ( ~ animal(H)
    | ~ owns(spaniard,H)
    | ~ owns(japanese,H) )).

cnf(norwegian_and_ukranian_own_different_pets,axiom,
    ( ~ animal(H)
    | ~ owns(norwegian,H)
    | ~ owns(ukranian,H) )).

cnf(norwegian_and_japanese_own_different_pets,axiom,
    ( ~ animal(H)
    | ~ owns(norwegian,H)
    | ~ owns(japanese,H) )).

cnf(ukranian_and_japanese_own_different_pets,axiom,
    ( ~ animal(H)
    | ~ owns(ukranian,H)
    | ~ owns(japanese,H) )).

%---- Houses are coloured
cnf(house_coloured,axiom,
    ( ~ house(H)
    | is_color(H,red)
    | is_color(H,yellow)
    | is_color(H,blue)
    | is_color(H,green)
    | is_color(H,ivory) )).

%---- uniqueness.
cnf(houses_1_and_2_coloured_different,axiom,
    ( ~ color(H)
    | ~ is_color(house_1,H)
    | ~ is_color(house_2,H) )).

cnf(houses_1_and_3_coloured_different,axiom,
    ( ~ color(H)
    | ~ is_color(house_1,H)
    | ~ is_color(house_3,H) )).

cnf(houses_1_and_4_coloured_different,axiom,
    ( ~ color(H)
    | ~ is_color(house_1,H)
    | ~ is_color(house_4,H) )).

cnf(houses_1_and_5_coloured_different,axiom,
    ( ~ color(H)
    | ~ is_color(house_1,H)
    | ~ is_color(house_5,H) )).

cnf(houses_2_and_3_coloured_different,axiom,
    ( ~ color(H)
    | ~ is_color(house_2,H)
    | ~ is_color(house_3,H) )).

cnf(houses_2_and_4_coloured_different,axiom,
    ( ~ color(H)
    | ~ is_color(house_2,H)
    | ~ is_color(house_4,H) )).

cnf(houses_2_and_5_coloured_different,axiom,
    ( ~ color(H)
    | ~ is_color(house_2,H)
    | ~ is_color(house_5,H) )).

cnf(houses_3_and_4_coloured_different,axiom,
    ( ~ color(H)
    | ~ is_color(house_3,H)
    | ~ is_color(house_4,H) )).

cnf(houses_3_and_5_coloured_different,axiom,
    ( ~ color(H)
    | ~ is_color(house_3,H)
    | ~ is_color(house_5,H) )).

cnf(houses_4_and_5_coloured_different,axiom,
    ( ~ color(H)
    | ~ is_color(house_4,H)
    | ~ is_color(house_5,H) )).

%---- These are the people
cnf(english,axiom,
    ( person(english) )).

cnf(spaniard,axiom,
    ( person(spaniard) )).

cnf(norwegian,axiom,
    ( person(norwegian) )).

cnf(ukranian,axiom,
    ( person(ukranian) )).

cnf(japanese,axiom,
    ( person(japanese) )).

%---- These are the house numbers
cnf(house_house_1,axiom,
    ( house(house_1) )).

cnf(house_house_2,axiom,
    ( house(house_2) )).

cnf(house_house_3,axiom,
    ( house(house_3) )).

cnf(house_house_4,axiom,
    ( house(house_4) )).

cnf(house_house_5,axiom,
    ( house(house_5) )).

%---- These are the colours
cnf(red,axiom,
    ( color(red) )).

cnf(green,axiom,
    ( color(green) )).

cnf(yellow,axiom,
    ( color(yellow) )).

cnf(ivory,axiom,
    ( color(ivory) )).

cnf(blue,axiom,
    ( color(blue) )).

%---- These are the cars
cnf(jaguar,axiom,
    ( car(jaguar) )).

cnf(honda,axiom,
    ( car(honda) )).

cnf(masserati,axiom,
    ( car(masserati) )).

cnf(porsche,axiom,
    ( car(porsche) )).

cnf(saab,axiom,
    ( car(saab) )).

%---- These are the drinks
cnf(tea,axiom,
    ( drink(tea) )).

cnf(orange,axiom,
    ( drink(orange) )).

cnf(water,axiom,
    ( drink(water) )).

cnf(milk,axiom,
    ( drink(milk) )).

cnf(coffee,axiom,
    ( drink(coffee) )).

%---- These are the pets
cnf(dog,axiom,
    ( animal(dog) )).

cnf(zebra,axiom,
    ( animal(zebra) )).

cnf(snails,axiom,
    ( animal(snails) )).

cnf(horse,axiom,
    ( animal(horse) )).

cnf(fox,axiom,
    ( animal(fox) )).

%---- Constraints.
%---- the englishman lives in the red house.
cnf(english_in_red_house,axiom,
    ( is_color(H,red)
    | ~ house(H)
    | ~ lives(english,H) )).

%---- the spaniard owns dog.
cnf(spaniard_owns_dog,axiom,
    ( owns(spaniard,dog) )).

%---- the norwegian lives in the first house.
cnf(norwegian_in_house_house_1,axiom,
    ( lives(norwegian,house_1) )).

%---- masserati is driven in the yellow house.
cnf(masserati_in_yellow_house,axiom,
    ( is_color(H,yellow)
    | ~ person(Person)
    | ~ drives(Person,masserati)
    | ~ house(H)
    | ~ lives(Person,H) )).

%---- saab is driven next to where the fox is kept.
cnf(saab_with_fox,axiom,
    ( next_to(House_1,House_2)
    | ~ person(Person_1)
    | ~ owns(Person_1,fox)
    | ~ house(House_1)
    | ~ lives(Person_1,House_1)
    | ~ person(Person_2)
    | ~ drives(Person_2,saab)
    | ~ house(House_2)
    | ~ lives(Person_2,House_2) )).

%---- the norwegian lives next to the blue house.
cnf(norwegian_in_blue_house,axiom,
    ( is_color(House_2,blue)
    | ~ house(House_1)
    | ~ lives(norwegian,House_1)
    | ~ house(House_2)
    | ~ next_to(House_1,House_2) )).

%---- the porsche driver owns snails.
cnf(porsche_with_snails,axiom,
    ( owns(P,snails)
    | ~ person(P)
    | ~ drives(P,porsche) )).

%---- the honda driver drinks orange juice.
cnf(honda_with_orange,axiom,
    ( drinks(P,orange)
    | ~ person(P)
    | ~ drives(P,honda) )).

%---- the ukranian drinks tea.
cnf(ukranian_drinks_tea,axiom,
    ( drinks(ukranian,tea) )).

%---- the japanese drives a jaguar.
cnf(japanese_drives_jaguar,axiom,
    ( drives(japanese,jaguar) )).

%---- the masserati driver lives next to where the horse is kept.
cnf(masserati_next_to_horse,axiom,
    ( next_to(House_1,House_2)
    | ~ person(Person_1)
    | ~ drives(Person_1,masserati)
    | ~ house(House_1)
    | ~ lives(Person_1,House_1)
    | ~ person(Person_2)
    | ~ owns(Person_2,horse)
    | ~ house(House_2)
    | ~ lives(Person_2,House_2) )).

%---- coffee is drunk in the green house.
cnf(coffee_in_green_house,axiom,
    ( is_color(H,green)
    | ~ person(P)
    | ~ drinks(P,coffee)
    | ~ house(H)
    | ~ lives(P,H) )).

%---- the green house is to the immediate right of the ivory house.
cnf(green_right_of_ivory,axiom,
    ( left_of(House_2,House_1)
    | ~ house(House_1)
    | ~ is_color(House_1,green)
    | ~ house(House_2)
    | ~ is_color(House_2,ivory) )).

%---- milk is drunk in the middle house.
cnf(milk_in_middle,axiom,
    ( lives(P,house_3)
    | ~ person(P)
    | ~ drinks(P,milk) )).

%---- axioms for next.
cnf(left_means_next_to,axiom,
    ( next_to(X,Y)
    | ~ left_of(X,Y) )).

cnf(right_mean_next_to,axiom,
    ( next_to(X,Y)
    | ~ left_of(Y,X) )).

cnf(next_to_means_left_or_right,axiom,
    ( left_of(X,Y)
    | ~ next_to(X,Y)
    | left_of(Y,X) )).

cnf(house_1_left_of_house_2,axiom,
    ( left_of(house_1,house_2) )).

cnf(house_2_left_of_house_3,axiom,
    ( left_of(house_2,house_3) )).

cnf(house_3_left_of_house_4,axiom,
    ( left_of(house_3,house_4) )).

cnf(house_4_left_of_house_5,axiom,
    ( left_of(house_4,house_5) )).

cnf(house_1_not_left_of_house_1,axiom,
    ( ~ left_of(house_1,house_1) )).

cnf(house_2_not_left_of_house_1,axiom,
    ( ~ left_of(house_2,house_1) )).

cnf(house_3_not_left_of_house_1,axiom,
    ( ~ left_of(house_3,house_1) )).

cnf(house_4_not_left_of_house_1,axiom,
    ( ~ left_of(house_4,house_1) )).

cnf(house_5_not_left_of_house_1,axiom,
    ( ~ left_of(house_5,house_1) )).

cnf(house_2_not_left_of_house_2,axiom,
    ( ~ left_of(house_2,house_2) )).

cnf(house_3_not_left_of_house_2,axiom,
    ( ~ left_of(house_3,house_2) )).

cnf(house_4_not_left_of_house_2,axiom,
    ( ~ left_of(house_4,house_2) )).

cnf(house_5_not_left_of_house_2,axiom,
    ( ~ left_of(house_5,house_2) )).

cnf(house_1_not_left_of_house_3,axiom,
    ( ~ left_of(house_1,house_3) )).

cnf(house_3_not_left_of_house_3,axiom,
    ( ~ left_of(house_3,house_3) )).

cnf(house_4_not_left_of_house_3,axiom,
    ( ~ left_of(house_4,house_3) )).

cnf(house_5_not_left_of_house_3,axiom,
    ( ~ left_of(house_5,house_3) )).

cnf(house_1_not_left_of_house_4,axiom,
    ( ~ left_of(house_1,house_4) )).

cnf(house_2_not_left_of_house_4,axiom,
    ( ~ left_of(house_2,house_4) )).

cnf(house_4_not_left_of_house_4,axiom,
    ( ~ left_of(house_4,house_4) )).

cnf(house_5_not_left_of_house_4,axiom,
    ( ~ left_of(house_5,house_4) )).

cnf(house_1_not_left_of_house_5,axiom,
    ( ~ left_of(house_1,house_5) )).

cnf(house_2_not_left_of_house_5,axiom,
    ( ~ left_of(house_2,house_5) )).

cnf(house_3_not_left_of_house_5,axiom,
    ( ~ left_of(house_3,house_5) )).

cnf(house_5_not_left_of_house_5,axiom,
    ( ~ left_of(house_5,house_5) )).

%---- negation of goal.
cnf(prove_configuration,negated_conjecture,
    ( ~ drinks(norwegian,water)
    | ~ drinks(ukranian,tea)
    | ~ drinks(japanese,coffee)
    | ~ drinks(english,milk)
    | ~ drinks(spaniard,orange)
    | ~ owns(norwegian,fox)
    | ~ owns(ukranian,horse)
    | ~ owns(japanese,zebra)
    | ~ owns(english,snails)
    | ~ owns(spaniard,dog)
    | ~ drives(norwegian,masserati)
    | ~ drives(ukranian,saab)
    | ~ drives(japanese,jaguar)
    | ~ drives(english,porsche)
    | ~ drives(spaniard,honda)
    | ~ lives(norwegian,house_1)
    | ~ lives(ukranian,house_2)
    | ~ lives(japanese,house_5)
    | ~ lives(english,house_3)
    | ~ lives(spaniard,house_4)
    | ~ is_color(house_1,yellow)
    | ~ is_color(house_2,blue)
    | ~ is_color(house_3,red)
    | ~ is_color(house_4,ivory)
    | ~ is_color(house_5,green) )).

%--------------------------------------------------------------------------
