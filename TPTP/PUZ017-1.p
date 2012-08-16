%--------------------------------------------------------------------------
% File     : PUZ017-1 : TPTP v5.2.0. Released v1.0.0.
% Domain   : Puzzles
% Problem  : The Houses
% Version  : Especial.
% English  : There are 5 houses, 5 people, 5 colors, 5 drinks, 5 games,
%            and 4 pets. Each house has a person, a color, a drink, and
%            game, and all but one of the houses has a pet. The problem
%            is to match each house with as many properties as possible.
%             House 1 is at the left end and house 5 is at the right end.
%             The Englishman lives in the Red house. The white house
%            is left of the Green house. The Italian has a Guppy. Lemonade
%            is drunk in the Green house. The Swede lives in the house
%            where Coffee is drunk. The Toad lives in the house where
%            Backgammon is played. Racquetball is played in the yellow
%            house. Milk is drunk in the third house. The Russian lives
%            in the first house. The Camel lives next to the house where
%            Quoits is played. The Rat lives next to the house where
%            Racquetball is played. Solitaire is played in the house where
%            vodka is drunk. The American lives in the house where
%            Charades is played. The Russian lives next to the Blue house.

% Refs     :
% Source   : [ANL]
% Names    : houses.ver1.in [ANL]

% Status   : Unsatisfiable
% Rating   : 0.00 v4.0.1, 0.20 v3.7.0, 0.00 v3.1.0, 0.11 v2.7.0, 0.17 v2.6.0, 0.44 v2.5.0, 0.25 v2.4.0, 0.75 v2.3.0, 0.67 v2.2.1, 1.00 v2.0.0
% Syntax   : Number of clauses     :  148 (  17 non-Horn;  82 unit; 132 RR)
%            Number of atoms       :  332 (   0 equality)
%            Maximal clause size   :   25 (   2 average)
%            Number of predicates  :   13 (   0 propositional; 2-2 arity)
%            Number of functors    :   30 (  30 constant; 0-0 arity)
%            Number of variables   :  155 (  27 singleton)
%            Maximal term depth    :    1 (   1 average)
% SPC      : CNF_UNS_EPR

% Comments :
%--------------------------------------------------------------------------
cnf(reflexivity_for_samehouse,axiom,
    ( samehouse(X,X) )).

cnf(house_1_not_2,axiom,
    ( ~ samehouse(n1,n2) )).

cnf(house_1_not_3,axiom,
    ( ~ samehouse(n1,n3) )).

cnf(house_1_not_4,axiom,
    ( ~ samehouse(n1,n4) )).

cnf(house_1_not_5,axiom,
    ( ~ samehouse(n1,n5) )).

cnf(house_2_not_3,axiom,
    ( ~ samehouse(n2,n3) )).

cnf(house_2_not_4,axiom,
    ( ~ samehouse(n2,n4) )).

cnf(house_2_not_5,axiom,
    ( ~ samehouse(n2,n5) )).

cnf(house_3_not_4,axiom,
    ( ~ samehouse(n3,n4) )).

cnf(house_3_not_5,axiom,
    ( ~ samehouse(n3,n5) )).

cnf(house_4_not_5,axiom,
    ( ~ samehouse(n4,n5) )).

cnf(reflexivity_for_sameperson,axiom,
    ( sameperson(X,X) )).

cnf(englishman_not_italian,axiom,
    ( ~ sameperson(englishman,italian) )).

cnf(englishman_not_swede,axiom,
    ( ~ sameperson(englishman,swede) )).

cnf(englishman_not_russian,axiom,
    ( ~ sameperson(englishman,russian) )).

cnf(englishman_not_american,axiom,
    ( ~ sameperson(englishman,american) )).

cnf(italian_not_swede,axiom,
    ( ~ sameperson(italian,swede) )).

cnf(italian_not_russian,axiom,
    ( ~ sameperson(italian,russian) )).

cnf(italian_not_american,axiom,
    ( ~ sameperson(italian,american) )).

cnf(swede_not_russian,axiom,
    ( ~ sameperson(swede,russian) )).

cnf(swede_not_american,axiom,
    ( ~ sameperson(swede,american) )).

cnf(russian_not_american,axiom,
    ( ~ sameperson(russian,american) )).

cnf(reflexivity_for_samecolor,axiom,
    ( samecolor(X,X) )).

cnf(red_not_white,axiom,
    ( ~ samecolor(red,white) )).

cnf(red_not_green,axiom,
    ( ~ samecolor(red,green) )).

cnf(red_not_yellow,axiom,
    ( ~ samecolor(red,yellow) )).

cnf(red_not_blue,axiom,
    ( ~ samecolor(red,blue) )).

cnf(white_not_green,axiom,
    ( ~ samecolor(white,green) )).

cnf(white_not_yellow,axiom,
    ( ~ samecolor(white,yellow) )).

cnf(white_not_blue,axiom,
    ( ~ samecolor(white,blue) )).

cnf(green_not_yellow,axiom,
    ( ~ samecolor(green,yellow) )).

cnf(green_not_blue,axiom,
    ( ~ samecolor(green,blue) )).

cnf(yellow_not_blue,axiom,
    ( ~ samecolor(yellow,blue) )).

cnf(reflexivity_for_samedrink,axiom,
    ( samedrink(X,X) )).

cnf(lemonade_not_coffee,axiom,
    ( ~ samedrink(lemonade,coffee) )).

cnf(lemonade_not_milk,axiom,
    ( ~ samedrink(lemonade,milk) )).

cnf(lemonade_not_vodka,axiom,
    ( ~ samedrink(lemonade,vodka) )).

cnf(lemonade_not_unknown,axiom,
    ( ~ samedrink(lemonade,unknown_drink) )).

cnf(coffee_not_milk,axiom,
    ( ~ samedrink(coffee,milk) )).

cnf(coffee_not_vodka,axiom,
    ( ~ samedrink(coffee,vodka) )).

cnf(coffee_not_known,axiom,
    ( ~ samedrink(coffee,unknown_drink) )).

cnf(milk_not_vodka,axiom,
    ( ~ samedrink(milk,vodka) )).

cnf(milk_not_unknown,axiom,
    ( ~ samedrink(milk,unknown_drink) )).

cnf(vodka_not_unknown,axiom,
    ( ~ samedrink(vodka,unknown_drink) )).

cnf(reflexivity_for_samegame,axiom,
    ( samegame(X,X) )).

cnf(backgammon_not_recquetball,axiom,
    ( ~ samegame(backgammon,racquetball) )).

cnf(backgammon_not_quoits,axiom,
    ( ~ samegame(backgammon,quoits) )).

cnf(backgammon_not_solitaire,axiom,
    ( ~ samegame(backgammon,solitaire) )).

cnf(backgammon_not_charades,axiom,
    ( ~ samegame(backgammon,charades) )).

cnf(racquetball_not_quoits,axiom,
    ( ~ samegame(racquetball,quoits) )).

cnf(racquetball_not_solitaire,axiom,
    ( ~ samegame(racquetball,solitaire) )).

cnf(racquetball_not_charades,axiom,
    ( ~ samegame(racquetball,charades) )).

cnf(quoits_not_solitaire,axiom,
    ( ~ samegame(quoits,solitaire) )).

cnf(quoits_not_charades,axiom,
    ( ~ samegame(quoits,charades) )).

cnf(solitaire_not_charades,axiom,
    ( ~ samegame(solitaire,charades) )).

cnf(reflexivity_for_samepet,axiom,
    ( samepet(X,X) )).

cnf(guppy_not_toad,axiom,
    ( ~ samepet(guppy,toad) )).

cnf(guppy_not_camel,axiom,
    ( ~ samepet(guppy,camel) )).

cnf(guppy_not_rat,axiom,
    ( ~ samepet(guppy,rat) )).

cnf(guppy_is_pet,axiom,
    ( ~ samepet(guppy,no_pet) )).

cnf(toad_not_camel,axiom,
    ( ~ samepet(toad,camel) )).

cnf(toad_not_rat,axiom,
    ( ~ samepet(toad,rat) )).

cnf(toad_is_pet,axiom,
    ( ~ samepet(toad,no_pet) )).

cnf(camel_not_rat,axiom,
    ( ~ samepet(camel,rat) )).

cnf(camel_is_pet,axiom,
    ( ~ samepet(camel,no_pet) )).

cnf(rat_is_pet,axiom,
    ( ~ samepet(rat,no_pet) )).

cnf(symmetry_of_nextto,axiom,
    ( ~ nextto(X,Y)
    | nextto(Y,X) )).

cnf(non_symmetry_of_left,axiom,
    ( ~ left(X,Y)
    | ~ left(Y,X) )).

cnf(nextto_means_left,axiom,
    ( ~ nextto(X,Y)
    | left(X,Y)
    | left(Y,X) )).

cnf(left_means_nextto,axiom,
    ( ~ left(X,Y)
    | nextto(X,Y) )).

cnf(house_not_nextto_itself,axiom,
    ( ~ samehouse(X,Y)
    | ~ nextto(X,Y) )).

cnf(nothing_left_of_itself,axiom,
    ( ~ left(X,X) )).

cnf(nothing_nextto_itself,axiom,
    ( ~ nextto(X,X) )).

cnf(every_house_has_a_national,axiom,
    ( hasperson(X,englishman)
    | hasperson(X,italian)
    | hasperson(X,swede)
    | hasperson(X,russian)
    | hasperson(X,american) )).

cnf(every_natioality_is_used,axiom,
    ( hasperson(n1,Y)
    | hasperson(n2,Y)
    | hasperson(n3,Y)
    | hasperson(n4,Y)
    | hasperson(n5,Y) )).

cnf(every_house_has_color,axiom,
    ( hascolor(X,red)
    | hascolor(X,white)
    | hascolor(X,green)
    | hascolor(X,yellow)
    | hascolor(X,blue) )).

cnf(every_color_is_used,axiom,
    ( hascolor(n1,Y)
    | hascolor(n2,Y)
    | hascolor(n3,Y)
    | hascolor(n4,Y)
    | hascolor(n5,Y) )).

cnf(every_house_has_a_drink,axiom,
    ( hasdrink(X,lemonade)
    | hasdrink(X,coffee)
    | hasdrink(X,milk)
    | hasdrink(X,vodka)
    | hasdrink(X,unknown_drink) )).

cnf(every_drink_is_used,axiom,
    ( hasdrink(n1,Y)
    | hasdrink(n2,Y)
    | hasdrink(n3,Y)
    | hasdrink(n4,Y)
    | hasdrink(n5,Y) )).

cnf(every_house_has_a_game,axiom,
    ( hasgame(X,backgammon)
    | hasgame(X,racquetball)
    | hasgame(X,quoits)
    | hasgame(X,solitaire)
    | hasgame(X,charades) )).

cnf(every_game_is_used,axiom,
    ( hasgame(n1,Y)
    | hasgame(n2,Y)
    | hasgame(n3,Y)
    | hasgame(n4,Y)
    | hasgame(n5,Y) )).

cnf(every_house_has_a_pet,axiom,
    ( haspet(X,guppy)
    | haspet(X,toad)
    | haspet(X,camel)
    | haspet(X,rat)
    | haspet(X,no_pet) )).

cnf(every_pet_is_used,axiom,
    ( haspet(n1,Y)
    | haspet(n2,Y)
    | haspet(n3,Y)
    | haspet(n4,Y)
    | haspet(n5,Y) )).

cnf(houses_have_unique_colors,axiom,
    ( samehouse(X1,X2)
    | ~ hascolor(X1,Y)
    | ~ hascolor(X2,Y) )).

cnf(nationals_have_unique_houses,axiom,
    ( samehouse(X1,X2)
    | ~ hasperson(X1,Y)
    | ~ hasperson(X2,Y) )).

cnf(drinks_have_unique_houses,axiom,
    ( samehouse(X1,X2)
    | ~ hasdrink(X1,Y)
    | ~ hasdrink(X2,Y) )).

cnf(games_have_unique_houses,axiom,
    ( samehouse(X1,X2)
    | ~ hasgame(X1,Y)
    | ~ hasgame(X2,Y) )).

cnf(pets_have_unique_houses,axiom,
    ( samehouse(X1,X2)
    | ~ haspet(X1,Y)
    | ~ haspet(X2,Y) )).

cnf(houses_have_unique_nationals,axiom,
    ( sameperson(X1,X2)
    | ~ hasperson(Y,X1)
    | ~ hasperson(Y,X2) )).

cnf(colours_are_unique,axiom,
    ( samecolor(X1,X2)
    | ~ hascolor(Y,X1)
    | ~ hascolor(Y,X2) )).

cnf(houses_have_unique_drinks,axiom,
    ( samedrink(X1,X2)
    | ~ hasdrink(Y,X1)
    | ~ hasdrink(Y,X2) )).

cnf(houses_have_unique_games,axiom,
    ( samegame(X1,X2)
    | ~ hasgame(Y,X1)
    | ~ hasgame(Y,X2) )).

cnf(houses_have_unique_pets,axiom,
    ( samepet(X1,X2)
    | ~ haspet(Y,X1)
    | ~ haspet(Y,X2) )).

cnf(englishman_lives_in_red_house1,hypothesis,
    ( ~ hasperson(X,englishman)
    | hascolor(X,red) )).

cnf(englishman_lives_in_red_house2,hypothesis,
    ( hasperson(X,englishman)
    | ~ hascolor(X,red) )).

cnf(white_house_left_of_green1,hypothesis,
    ( ~ hascolor(X,white)
    | ~ hascolor(Y,green)
    | left(X,Y) )).

cnf(white_house_left_of_green2,hypothesis,
    ( ~ hascolor(X,white)
    | hascolor(Y,green)
    | ~ left(X,Y) )).

cnf(white_house_left_of_green3,hypothesis,
    ( hascolor(X,white)
    | ~ hascolor(Y,green)
    | ~ left(X,Y) )).

cnf(italian_has_guppy1,hypothesis,
    ( ~ hasperson(X,italian)
    | haspet(X,guppy) )).

cnf(italian_has_guppy2,hypothesis,
    ( hasperson(X,italian)
    | ~ haspet(X,guppy) )).

cnf(lemonade_in_green_house1,hypothesis,
    ( ~ hasdrink(X,lemonade)
    | hascolor(X,green) )).

cnf(lemonade_in_green_house2,hypothesis,
    ( hasdrink(X,lemonade)
    | ~ hascolor(X,green) )).

cnf(swede_drinks_coffee1,hypothesis,
    ( ~ hasperson(X,swede)
    | hasdrink(X,coffee) )).

cnf(swede_drinks_coffee2,hypothesis,
    ( hasperson(X,swede)
    | ~ hasdrink(X,coffee) )).

cnf(toad_lives_with_backgammon1,hypothesis,
    ( ~ haspet(X,toad)
    | hasgame(X,backgammon) )).

cnf(toad_lives_with_backgammon2,hypothesis,
    ( haspet(X,toad)
    | ~ hasgame(X,backgammon) )).

cnf(racquetball_played_in_yellow_house1,hypothesis,
    ( ~ hasgame(X,racquetball)
    | hascolor(X,yellow) )).

cnf(racquetball_played_in_yellow_house2,hypothesis,
    ( hasgame(X,racquetball)
    | ~ hascolor(X,yellow) )).

cnf(c1,hypothesis,
    ( ~ haspet(X,camel)
    | samehouse(Y,Z)
    | ~ nextto(X,Y)
    | ~ nextto(X,Z)
    | hasgame(Y,quoits)
    | hasgame(Z,quoits) )).

cnf(c2,hypothesis,
    ( ~ haspet(X,camel)
    | ~ samehouse(n1,X)
    | ~ nextto(X,Y)
    | hasgame(Y,quoits) )).

cnf(c3,hypothesis,
    ( ~ haspet(X,camel)
    | ~ samehouse(X,n5)
    | ~ nextto(X,Y)
    | hasgame(Y,quoits) )).

cnf(c4,hypothesis,
    ( ~ haspet(X,camel)
    | nextto(X,Y)
    | ~ hasgame(Y,quoits) )).

cnf(c5,hypothesis,
    ( samehouse(Y,Z)
    | ~ nextto(X,Y)
    | ~ nextto(X,Z)
    | ~ hasgame(X,quoits)
    | haspet(Y,camel)
    | haspet(Z,camel) )).

cnf(c6,hypothesis,
    ( ~ samehouse(n1,X)
    | ~ nextto(X,Y)
    | ~ hasgame(X,quoits)
    | haspet(Y,camel) )).

cnf(c7,hypothesis,
    ( ~ samehouse(X,n5)
    | ~ nextto(X,Y)
    | ~ hasgame(X,quoits)
    | haspet(Y,camel) )).

cnf(c8,hypothesis,
    ( ~ haspet(X,rat)
    | samehouse(Y,Z)
    | ~ nextto(X,Y)
    | ~ nextto(X,Z)
    | hasgame(Y,racquetball)
    | hasgame(Z,racquetball) )).

cnf(c9,hypothesis,
    ( ~ haspet(X,rat)
    | ~ nextto(X,Y)
    | ~ samehouse(n1,X)
    | hasgame(Y,racquetball) )).

cnf(c10,hypothesis,
    ( ~ haspet(X,rat)
    | ~ nextto(X,Y)
    | ~ samehouse(X,n5)
    | hasgame(Y,racquetball) )).

cnf(c11,hypothesis,
    ( ~ haspet(X,rat)
    | nextto(X,Y)
    | ~ hasgame(Y,racquetball) )).

cnf(c12,hypothesis,
    ( samehouse(Y,Z)
    | ~ nextto(X,Y)
    | ~ nextto(X,Z)
    | ~ hasgame(X,racquetball)
    | haspet(Y,rat)
    | haspet(Z,rat) )).

cnf(c13,hypothesis,
    ( ~ nextto(X,Y)
    | ~ samehouse(n1,X)
    | ~ hasgame(X,racquetball)
    | haspet(Y,rat) )).

cnf(c14,hypothesis,
    ( ~ nextto(X,Y)
    | ~ samehouse(X,n5)
    | ~ hasgame(X,racquetball)
    | haspet(Y,rat) )).

cnf(c15,hypothesis,
    ( ~ hasgame(X,solitaire)
    | hasdrink(X,vodka) )).

cnf(c16,hypothesis,
    ( hasgame(X,solitaire)
    | ~ hasdrink(X,vodka) )).

cnf(c17,hypothesis,
    ( ~ hasperson(X,american)
    | hasgame(X,charades) )).

cnf(c18,hypothesis,
    ( hasperson(X,american)
    | ~ hasgame(X,charades) )).

cnf(c19,hypothesis,
    ( ~ hasperson(X,russian)
    | samehouse(Y,Z)
    | ~ nextto(X,Y)
    | ~ nextto(X,Z)
    | hascolor(Y,blue)
    | hascolor(Z,blue) )).

cnf(c20,hypothesis,
    ( ~ hasperson(X,russian)
    | ~ samehouse(n1,X)
    | ~ nextto(X,Y)
    | hascolor(Y,blue) )).

cnf(c21,hypothesis,
    ( ~ hasperson(X,russian)
    | ~ samehouse(X,n5)
    | ~ nextto(X,Y)
    | hascolor(Y,blue) )).

cnf(c22,hypothesis,
    ( ~ hasperson(X,russian)
    | nextto(X,Y)
    | ~ hascolor(Y,blue) )).

cnf(c23,hypothesis,
    ( samehouse(Y,Z)
    | ~ nextto(X,Y)
    | ~ nextto(X,Z)
    | ~ hascolor(X,blue)
    | hasperson(Y,russian)
    | hasperson(Z,russian) )).

cnf(c24,hypothesis,
    ( ~ nextto(X,Y)
    | ~ hascolor(X,blue)
    | ~ samehouse(n1,X)
    | hasperson(Y,russian) )).

cnf(c25,hypothesis,
    ( ~ nextto(X,Y)
    | ~ hascolor(X,blue)
    | ~ samehouse(X,n5)
    | hasperson(Y,russian) )).

cnf(house1_at_left,hypothesis,
    ( ~ left(X,n1) )).

cnf(house5_at_right,hypothesis,
    ( ~ left(n5,X) )).

cnf(house_1_left_of_2,hypothesis,
    ( left(n1,n2) )).

cnf(house_2_left_of_3,hypothesis,
    ( left(n2,n3) )).

cnf(house_3_left_of_4,hypothesis,
    ( left(n3,n4) )).

cnf(house_4_left_of_5,hypothesis,
    ( left(n4,n5) )).

cnf(house_1_not_nextto_3,hypothesis,
    ( ~ nextto(n1,n3) )).

cnf(house_1_not_nextto_4,hypothesis,
    ( ~ nextto(n1,n4) )).

cnf(house_1_not_nextto_5,hypothesis,
    ( ~ nextto(n1,n5) )).

cnf(house_2_not_nextto_4,hypothesis,
    ( ~ nextto(n2,n4) )).

cnf(house_2_not_nextto_5,hypothesis,
    ( ~ nextto(n2,n5) )).

cnf(house_3_not_nextto_5,hypothesis,
    ( ~ nextto(n3,n5) )).

cnf(house_3_has_milk,hypothesis,
    ( hasdrink(n3,milk) )).

cnf(house_1_has_russian,hypothesis,
    ( hasperson(n1,russian) )).

cnf(find_out_house_details,negated_conjecture,
    ( ~ hasperson(n1,X0)
    | ~ hasperson(n2,X1)
    | ~ hasperson(n3,X2)
    | ~ hasperson(n4,X3)
    | ~ hasperson(n5,X4)
    | ~ hascolor(n1,X5)
    | ~ hascolor(n2,X6)
    | ~ hascolor(n3,X7)
    | ~ hascolor(n4,X8)
    | ~ hascolor(n5,X9)
    | ~ hasdrink(n1,X10)
    | ~ hasdrink(n2,X11)
    | ~ hasdrink(n3,X11a)
    | ~ hasdrink(n4,X12)
    | ~ hasdrink(n5,X13)
    | ~ hasgame(n1,X14)
    | ~ hasgame(n2,X15)
    | ~ hasgame(n3,X16)
    | ~ hasgame(n4,X17)
    | ~ hasgame(n5,X18)
    | ~ haspet(n1,X19)
    | ~ haspet(n2,X20)
    | ~ haspet(n3,X21)
    | ~ haspet(n4,X22)
    | ~ haspet(n5,X23) )).

%--------------------------------------------------------------------------
