%--------------------------------------------------------------------------
% File     : PUZ015-1 : TPTP v5.2.0. Released v1.0.0.
% Domain   : Puzzles
% Problem  : Checkerboard and Dominoes : Opposing corners removed
% Version  : Especial.
% English  : There is a checker board whose upper left and lower right
%            squares have been removed. There is a box of dominoes that
%            are one square by two squares in size. Can you exactly cover
%            the checker board with dominoes?

% Refs     :
% Source   : [ANL]
% Names    : chekndom.ver1.in [ANL]

% Status   : Satisfiable
% Rating   : 0.89 v5.2.0, 0.90 v5.0.0, 0.89 v4.1.0, 0.86 v4.0.1, 1.00 v4.0.0, 0.75 v3.7.0, 0.67 v3.4.0, 0.75 v3.3.0, 0.67 v3.2.0, 0.80 v3.1.0, 0.67 v2.7.0, 0.33 v2.6.0, 1.00 v2.0.0
% Syntax   : Number of clauses     :   21 (   0 non-Horn;  13 unit;  21 RR)
%            Number of atoms       :   29 (  11 equality)
%            Maximal clause size   :    2 (   1 average)
%            Number of predicates  :    2 (   0 propositional; 2-2 arity)
%            Number of functors    :   16 (  12 constant; 0-8 arity)
%            Number of variables   :   58 (   0 singleton)
%            Maximal term depth    :    3 (   2 average)
% SPC      : CNF_SAT_RFO_EQU_NUE

% Comments :
%--------------------------------------------------------------------------
cnf(cover_columns_1_and_2,axiom,
    ( ~ achievable(row(X),squares(not_covered,not_covered,Y3,Y4,Y5,Y6,Y7,Y8))
    | achievable(row(X),squares(covered,covered,Y3,Y4,Y5,Y6,Y7,Y8)) )).

cnf(cover_columns_2_and_3,axiom,
    ( ~ achievable(row(X),squares(Y1,not_covered,not_covered,Y4,Y5,Y6,Y7,Y8))
    | achievable(row(X),squares(Y1,covered,covered,Y4,Y5,Y6,Y7,Y8)) )).

cnf(cover_columns_3_and_4,axiom,
    ( ~ achievable(row(X),squares(Y1,Y2,not_covered,not_covered,Y5,Y6,Y7,Y8))
    | achievable(row(X),squares(Y1,Y2,covered,covered,Y5,Y6,Y7,Y8)) )).

cnf(cover_columns_4_and_5,axiom,
    ( ~ achievable(row(X),squares(Y1,Y2,Y3,not_covered,not_covered,Y6,Y7,Y8))
    | achievable(row(X),squares(Y1,Y2,Y3,covered,covered,Y6,Y7,Y8)) )).

cnf(cover_columns_5_and_6,axiom,
    ( ~ achievable(row(X),squares(Y1,Y2,Y3,Y4,not_covered,not_covered,Y7,Y8))
    | achievable(row(X),squares(Y1,Y2,Y3,Y4,covered,covered,Y7,Y8)) )).

cnf(cover_columns_6_and_7,axiom,
    ( ~ achievable(row(X),squares(Y1,Y2,Y3,Y4,Y5,not_covered,not_covered,Y8))
    | achievable(row(X),squares(Y1,Y2,Y3,Y4,Y5,covered,covered,Y8)) )).

cnf(cover_columns_7_and_8,axiom,
    ( ~ achievable(row(X),squares(Y1,Y2,Y3,Y4,Y5,Y6,not_covered,not_covered))
    | achievable(row(X),squares(Y1,Y2,Y3,Y4,Y5,Y6,covered,covered)) )).

cnf(place_vertical_pieces,axiom,
    ( ~ achievable(row(X),squares(Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8))
    | achievable(row(successor(X)),squares(complement(Y1),complement(Y2),complement(Y3),complement(Y4),complement(Y5),complement(Y6),complement(Y7),complement(Y8))) )).

cnf(successor_of_1_is_2,axiom,
    ( successor(n1) = n2 )).

cnf(successor_of_2_is_3,axiom,
    ( successor(n2) = n3 )).

cnf(successor_of_3_is_4,axiom,
    ( successor(n3) = n4 )).

cnf(successor_of_4_is_5,axiom,
    ( successor(n4) = n5 )).

cnf(successor_of_5_is_6,axiom,
    ( successor(n5) = n6 )).

cnf(successor_of_6_is_7,axiom,
    ( successor(n6) = n7 )).

cnf(successor_of_7_is_8,axiom,
    ( successor(n7) = n8 )).

cnf(successor_of_8_is_9,axiom,
    ( successor(n8) = n9 )).

cnf(complement_of_covered_is_not_covered,axiom,
    ( complement(covered) = not_covered )).

cnf(complement_of_not_covered_is_covered,axiom,
    ( complement(not_covered) = covered )).

cnf(complement_of_r_is_not_covered,axiom,
    ( complement(removed) = not_covered )).

cnf(first_row_initially,hypothesis,
    ( achievable(row(n1),squares(removed,not_covered,not_covered,not_covered,not_covered,not_covered,not_covered,not_covered)) )).

%----This clause is in the original, but it not sensible or needed
% input_clause(can_do_anything_to_row_9,hypothesis,
%     [++achievable(row(9),squares(Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8))]).

cnf(try_prove_row_8_can_be_covered,negated_conjecture,
    ( ~ achievable(row(n8),squares(covered,covered,covered,covered,covered,covered,covered,not_covered)) )).

%--------------------------------------------------------------------------
