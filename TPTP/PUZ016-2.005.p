%--------------------------------------------------------------------------
% File     : PUZ016-2.005 : TPTP v5.2.0. Released v1.2.0.
% Domain   : Puzzles
% Problem  : Checkerboard and Dominoes : Row 1, columns 2 and 3 removed
% Version  : [Sti93] axioms : Especial.
%            Theorem formulation : Propositional.
% English  : There is a checker board whose second and third squares from
%            the first row have been removed. There is a box of dominoes
%            that are one square by two squares in size. Can you exactly
%            cover the checker board with dominoes? The size is the
%            dimension of the checker board.

% Refs     : [Sti93] Stickel (1993), Email to G. Sutcliffe
% Source   : [Sti93]
% Names    : - [Sti93]

% Status   : Unsatisfiable
% Rating   : 0.00 v2.1.0
% Syntax   : Number of clauses     :  117 (  23 non-Horn;   6 unit; 117 RR)
%            Number of atoms       :  256 (   0 equality)
%            Maximal clause size   :    4 (   2 average)
%            Number of predicates  :   40 (  40 propositional; 0-0 arity)
%            Number of functors    :    0 (   0 constant; --- arity)
%            Number of variables   :    0 (   0 singleton)
%            Maximal term depth    :    0 (   0 average)
% SPC      : CNF_UNS_PRP

% Comments : horizontal(R,C) means there is a horizontal tile from (R,C)
%            to (R,C+1). vertical(R,C) means there is a vertical tile from
%            (R,C) to (R+1,C).
%          : tptp2X: -f tptp -s5 PUZ016-2.g
%--------------------------------------------------------------------------
cnf(uncovered1_1_2,negated_conjecture,
    ( ~ horizontal_1_2 )).

cnf(uncovered1_1_3,negated_conjecture,
    ( ~ horizontal_1_3 )).

cnf(uncovered2_1_2,negated_conjecture,
    ( ~ vertical_1_2 )).

cnf(uncovered2_1_3,negated_conjecture,
    ( ~ vertical_1_3 )).

cnf(uncovered3_1_2,negated_conjecture,
    ( ~ horizontal_1_1 )).

cnf(uncovered3_1_3,negated_conjecture,
    ( ~ horizontal_1_2 )).

cnf(covered_1_1,negated_conjecture,
    ( horizontal_1_1
    | vertical_1_1 )).

cnf(covered_1_4,negated_conjecture,
    ( horizontal_1_4
    | vertical_1_4
    | horizontal_1_3 )).

cnf(covered_1_5,negated_conjecture,
    ( vertical_1_5
    | horizontal_1_4 )).

cnf(covered_2_1,negated_conjecture,
    ( horizontal_2_1
    | vertical_2_1
    | vertical_1_1 )).

cnf(covered_2_2,negated_conjecture,
    ( horizontal_2_2
    | vertical_2_2
    | horizontal_2_1
    | vertical_1_2 )).

cnf(covered_2_3,negated_conjecture,
    ( horizontal_2_3
    | vertical_2_3
    | horizontal_2_2
    | vertical_1_3 )).

cnf(covered_2_4,negated_conjecture,
    ( horizontal_2_4
    | vertical_2_4
    | horizontal_2_3
    | vertical_1_4 )).

cnf(covered_2_5,negated_conjecture,
    ( vertical_2_5
    | horizontal_2_4
    | vertical_1_5 )).

cnf(covered_3_1,negated_conjecture,
    ( horizontal_3_1
    | vertical_3_1
    | vertical_2_1 )).

cnf(covered_3_2,negated_conjecture,
    ( horizontal_3_2
    | vertical_3_2
    | horizontal_3_1
    | vertical_2_2 )).

cnf(covered_3_3,negated_conjecture,
    ( horizontal_3_3
    | vertical_3_3
    | horizontal_3_2
    | vertical_2_3 )).

cnf(covered_3_4,negated_conjecture,
    ( horizontal_3_4
    | vertical_3_4
    | horizontal_3_3
    | vertical_2_4 )).

cnf(covered_3_5,negated_conjecture,
    ( vertical_3_5
    | horizontal_3_4
    | vertical_2_5 )).

cnf(covered_4_1,negated_conjecture,
    ( horizontal_4_1
    | vertical_4_1
    | vertical_3_1 )).

cnf(covered_4_2,negated_conjecture,
    ( horizontal_4_2
    | vertical_4_2
    | horizontal_4_1
    | vertical_3_2 )).

cnf(covered_4_3,negated_conjecture,
    ( horizontal_4_3
    | vertical_4_3
    | horizontal_4_2
    | vertical_3_3 )).

cnf(covered_4_4,negated_conjecture,
    ( horizontal_4_4
    | vertical_4_4
    | horizontal_4_3
    | vertical_3_4 )).

cnf(covered_4_5,negated_conjecture,
    ( vertical_4_5
    | horizontal_4_4
    | vertical_3_5 )).

cnf(covered_5_1,negated_conjecture,
    ( horizontal_5_1
    | vertical_4_1 )).

cnf(covered_5_2,negated_conjecture,
    ( horizontal_5_2
    | horizontal_5_1
    | vertical_4_2 )).

cnf(covered_5_3,negated_conjecture,
    ( horizontal_5_3
    | horizontal_5_2
    | vertical_4_3 )).

cnf(covered_5_4,negated_conjecture,
    ( horizontal_5_4
    | horizontal_5_3
    | vertical_4_4 )).

cnf(covered_5_5,negated_conjecture,
    ( horizontal_5_4
    | vertical_4_5 )).

cnf(unique_cover1_1_1,negated_conjecture,
    ( ~ horizontal_1_1
    | ~ vertical_1_1 )).

cnf(unique_cover1_1_4,negated_conjecture,
    ( ~ horizontal_1_4
    | ~ vertical_1_4 )).

cnf(unique_cover2_1_4,negated_conjecture,
    ( ~ horizontal_1_4
    | ~ horizontal_1_3 )).

cnf(unique_cover4_1_4,negated_conjecture,
    ( ~ vertical_1_4
    | ~ horizontal_1_3 )).

cnf(unique_cover4_1_5,negated_conjecture,
    ( ~ vertical_1_5
    | ~ horizontal_1_4 )).

cnf(unique_cover1_2_1,negated_conjecture,
    ( ~ horizontal_2_1
    | ~ vertical_2_1 )).

cnf(unique_cover3_2_1,negated_conjecture,
    ( ~ horizontal_2_1
    | ~ vertical_1_1 )).

cnf(unique_cover5_2_1,negated_conjecture,
    ( ~ vertical_2_1
    | ~ vertical_1_1 )).

cnf(unique_cover1_2_2,negated_conjecture,
    ( ~ horizontal_2_2
    | ~ vertical_2_2 )).

cnf(unique_cover2_2_2,negated_conjecture,
    ( ~ horizontal_2_2
    | ~ horizontal_2_1 )).

cnf(unique_cover3_2_2,negated_conjecture,
    ( ~ horizontal_2_2
    | ~ vertical_1_2 )).

cnf(unique_cover4_2_2,negated_conjecture,
    ( ~ vertical_2_2
    | ~ horizontal_2_1 )).

cnf(unique_cover5_2_2,negated_conjecture,
    ( ~ vertical_2_2
    | ~ vertical_1_2 )).

cnf(unique_cover6_2_2,negated_conjecture,
    ( ~ horizontal_2_1
    | ~ vertical_1_2 )).

cnf(unique_cover1_2_3,negated_conjecture,
    ( ~ horizontal_2_3
    | ~ vertical_2_3 )).

cnf(unique_cover2_2_3,negated_conjecture,
    ( ~ horizontal_2_3
    | ~ horizontal_2_2 )).

cnf(unique_cover3_2_3,negated_conjecture,
    ( ~ horizontal_2_3
    | ~ vertical_1_3 )).

cnf(unique_cover4_2_3,negated_conjecture,
    ( ~ vertical_2_3
    | ~ horizontal_2_2 )).

cnf(unique_cover5_2_3,negated_conjecture,
    ( ~ vertical_2_3
    | ~ vertical_1_3 )).

cnf(unique_cover6_2_3,negated_conjecture,
    ( ~ horizontal_2_2
    | ~ vertical_1_3 )).

cnf(unique_cover1_2_4,negated_conjecture,
    ( ~ horizontal_2_4
    | ~ vertical_2_4 )).

cnf(unique_cover2_2_4,negated_conjecture,
    ( ~ horizontal_2_4
    | ~ horizontal_2_3 )).

cnf(unique_cover3_2_4,negated_conjecture,
    ( ~ horizontal_2_4
    | ~ vertical_1_4 )).

cnf(unique_cover4_2_4,negated_conjecture,
    ( ~ vertical_2_4
    | ~ horizontal_2_3 )).

cnf(unique_cover5_2_4,negated_conjecture,
    ( ~ vertical_2_4
    | ~ vertical_1_4 )).

cnf(unique_cover6_2_4,negated_conjecture,
    ( ~ horizontal_2_3
    | ~ vertical_1_4 )).

cnf(unique_cover4_2_5,negated_conjecture,
    ( ~ vertical_2_5
    | ~ horizontal_2_4 )).

cnf(unique_cover5_2_5,negated_conjecture,
    ( ~ vertical_2_5
    | ~ vertical_1_5 )).

cnf(unique_cover6_2_5,negated_conjecture,
    ( ~ horizontal_2_4
    | ~ vertical_1_5 )).

cnf(unique_cover1_3_1,negated_conjecture,
    ( ~ horizontal_3_1
    | ~ vertical_3_1 )).

cnf(unique_cover3_3_1,negated_conjecture,
    ( ~ horizontal_3_1
    | ~ vertical_2_1 )).

cnf(unique_cover5_3_1,negated_conjecture,
    ( ~ vertical_3_1
    | ~ vertical_2_1 )).

cnf(unique_cover1_3_2,negated_conjecture,
    ( ~ horizontal_3_2
    | ~ vertical_3_2 )).

cnf(unique_cover2_3_2,negated_conjecture,
    ( ~ horizontal_3_2
    | ~ horizontal_3_1 )).

cnf(unique_cover3_3_2,negated_conjecture,
    ( ~ horizontal_3_2
    | ~ vertical_2_2 )).

cnf(unique_cover4_3_2,negated_conjecture,
    ( ~ vertical_3_2
    | ~ horizontal_3_1 )).

cnf(unique_cover5_3_2,negated_conjecture,
    ( ~ vertical_3_2
    | ~ vertical_2_2 )).

cnf(unique_cover6_3_2,negated_conjecture,
    ( ~ horizontal_3_1
    | ~ vertical_2_2 )).

cnf(unique_cover1_3_3,negated_conjecture,
    ( ~ horizontal_3_3
    | ~ vertical_3_3 )).

cnf(unique_cover2_3_3,negated_conjecture,
    ( ~ horizontal_3_3
    | ~ horizontal_3_2 )).

cnf(unique_cover3_3_3,negated_conjecture,
    ( ~ horizontal_3_3
    | ~ vertical_2_3 )).

cnf(unique_cover4_3_3,negated_conjecture,
    ( ~ vertical_3_3
    | ~ horizontal_3_2 )).

cnf(unique_cover5_3_3,negated_conjecture,
    ( ~ vertical_3_3
    | ~ vertical_2_3 )).

cnf(unique_cover6_3_3,negated_conjecture,
    ( ~ horizontal_3_2
    | ~ vertical_2_3 )).

cnf(unique_cover1_3_4,negated_conjecture,
    ( ~ horizontal_3_4
    | ~ vertical_3_4 )).

cnf(unique_cover2_3_4,negated_conjecture,
    ( ~ horizontal_3_4
    | ~ horizontal_3_3 )).

cnf(unique_cover3_3_4,negated_conjecture,
    ( ~ horizontal_3_4
    | ~ vertical_2_4 )).

cnf(unique_cover4_3_4,negated_conjecture,
    ( ~ vertical_3_4
    | ~ horizontal_3_3 )).

cnf(unique_cover5_3_4,negated_conjecture,
    ( ~ vertical_3_4
    | ~ vertical_2_4 )).

cnf(unique_cover6_3_4,negated_conjecture,
    ( ~ horizontal_3_3
    | ~ vertical_2_4 )).

cnf(unique_cover4_3_5,negated_conjecture,
    ( ~ vertical_3_5
    | ~ horizontal_3_4 )).

cnf(unique_cover5_3_5,negated_conjecture,
    ( ~ vertical_3_5
    | ~ vertical_2_5 )).

cnf(unique_cover6_3_5,negated_conjecture,
    ( ~ horizontal_3_4
    | ~ vertical_2_5 )).

cnf(unique_cover1_4_1,negated_conjecture,
    ( ~ horizontal_4_1
    | ~ vertical_4_1 )).

cnf(unique_cover3_4_1,negated_conjecture,
    ( ~ horizontal_4_1
    | ~ vertical_3_1 )).

cnf(unique_cover5_4_1,negated_conjecture,
    ( ~ vertical_4_1
    | ~ vertical_3_1 )).

cnf(unique_cover1_4_2,negated_conjecture,
    ( ~ horizontal_4_2
    | ~ vertical_4_2 )).

cnf(unique_cover2_4_2,negated_conjecture,
    ( ~ horizontal_4_2
    | ~ horizontal_4_1 )).

cnf(unique_cover3_4_2,negated_conjecture,
    ( ~ horizontal_4_2
    | ~ vertical_3_2 )).

cnf(unique_cover4_4_2,negated_conjecture,
    ( ~ vertical_4_2
    | ~ horizontal_4_1 )).

cnf(unique_cover5_4_2,negated_conjecture,
    ( ~ vertical_4_2
    | ~ vertical_3_2 )).

cnf(unique_cover6_4_2,negated_conjecture,
    ( ~ horizontal_4_1
    | ~ vertical_3_2 )).

cnf(unique_cover1_4_3,negated_conjecture,
    ( ~ horizontal_4_3
    | ~ vertical_4_3 )).

cnf(unique_cover2_4_3,negated_conjecture,
    ( ~ horizontal_4_3
    | ~ horizontal_4_2 )).

cnf(unique_cover3_4_3,negated_conjecture,
    ( ~ horizontal_4_3
    | ~ vertical_3_3 )).

cnf(unique_cover4_4_3,negated_conjecture,
    ( ~ vertical_4_3
    | ~ horizontal_4_2 )).

cnf(unique_cover5_4_3,negated_conjecture,
    ( ~ vertical_4_3
    | ~ vertical_3_3 )).

cnf(unique_cover6_4_3,negated_conjecture,
    ( ~ horizontal_4_2
    | ~ vertical_3_3 )).

cnf(unique_cover1_4_4,negated_conjecture,
    ( ~ horizontal_4_4
    | ~ vertical_4_4 )).

cnf(unique_cover2_4_4,negated_conjecture,
    ( ~ horizontal_4_4
    | ~ horizontal_4_3 )).

cnf(unique_cover3_4_4,negated_conjecture,
    ( ~ horizontal_4_4
    | ~ vertical_3_4 )).

cnf(unique_cover4_4_4,negated_conjecture,
    ( ~ vertical_4_4
    | ~ horizontal_4_3 )).

cnf(unique_cover5_4_4,negated_conjecture,
    ( ~ vertical_4_4
    | ~ vertical_3_4 )).

cnf(unique_cover6_4_4,negated_conjecture,
    ( ~ horizontal_4_3
    | ~ vertical_3_4 )).

cnf(unique_cover4_4_5,negated_conjecture,
    ( ~ vertical_4_5
    | ~ horizontal_4_4 )).

cnf(unique_cover5_4_5,negated_conjecture,
    ( ~ vertical_4_5
    | ~ vertical_3_5 )).

cnf(unique_cover6_4_5,negated_conjecture,
    ( ~ horizontal_4_4
    | ~ vertical_3_5 )).

cnf(unique_cover3_5_1,negated_conjecture,
    ( ~ horizontal_5_1
    | ~ vertical_4_1 )).

cnf(unique_cover2_5_2,negated_conjecture,
    ( ~ horizontal_5_2
    | ~ horizontal_5_1 )).

cnf(unique_cover3_5_2,negated_conjecture,
    ( ~ horizontal_5_2
    | ~ vertical_4_2 )).

cnf(unique_cover6_5_2,negated_conjecture,
    ( ~ horizontal_5_1
    | ~ vertical_4_2 )).

cnf(unique_cover2_5_3,negated_conjecture,
    ( ~ horizontal_5_3
    | ~ horizontal_5_2 )).

cnf(unique_cover3_5_3,negated_conjecture,
    ( ~ horizontal_5_3
    | ~ vertical_4_3 )).

cnf(unique_cover6_5_3,negated_conjecture,
    ( ~ horizontal_5_2
    | ~ vertical_4_3 )).

cnf(unique_cover2_5_4,negated_conjecture,
    ( ~ horizontal_5_4
    | ~ horizontal_5_3 )).

cnf(unique_cover3_5_4,negated_conjecture,
    ( ~ horizontal_5_4
    | ~ vertical_4_4 )).

cnf(unique_cover6_5_4,negated_conjecture,
    ( ~ horizontal_5_3
    | ~ vertical_4_4 )).

cnf(unique_cover6_5_5,negated_conjecture,
    ( ~ horizontal_5_4
    | ~ vertical_4_5 )).

%--------------------------------------------------------------------------
