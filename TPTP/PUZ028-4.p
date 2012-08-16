%--------------------------------------------------------------------------
% File     : PUZ028-4 : TPTP v5.2.0. Released v1.1.0.
% Domain   : Puzzles
% Problem  : People at a party
% Version  : [SETHEO] axioms : Especial.
%            Theorem formulation : Grounded axioms and simplified.
% English  : We can always choose 3 persons who are either familiar
%            with each other or not familiar with each other, from 6
%            persons who meet at a party.

% Refs     :
% Source   : [SETHEO]
% Names    : ramsey3a.lop [SETHEO]

% Status   : Satisfiable
% Rating   : 0.00 v2.4.0, 0.00 v2.0.0
% Syntax   : Number of clauses     :  270 (  30 non-Horn;   0 unit; 270 RR)
%            Number of atoms       :  780 (   0 equality)
%            Maximal clause size   :    3 (   3 average)
%            Number of predicates  :    2 (   0 propositional; 2-2 arity)
%            Number of functors    :    6 (   6 constant; 0-0 arity)
%            Number of variables   :    0 (   0 singleton)
%            Maximal term depth    :    1 (   1 average)
% SPC      : CNF_SAT_EPR

% Comments : This version is satisfiable because familiarity is not meant in
%            a symmetric way.
%--------------------------------------------------------------------------
%----    ++familiar(X,Y),
%----    ++not_familiar(X,Y),
%----All ground instances of the above clause:
%----X=1
%----Y-6
cnf(familiar_or_not_1_2,axiom,
    ( familiar(n1,n2)
    | not_familiar(n1,n2) )).

cnf(familiar_or_not_1_3,axiom,
    ( familiar(n1,n3)
    | not_familiar(n1,n3) )).

cnf(familiar_or_not_1_4,axiom,
    ( familiar(n1,n4)
    | not_familiar(n1,n4) )).

cnf(familiar_or_not_1_5,axiom,
    ( familiar(n1,n5)
    | not_familiar(n1,n5) )).

cnf(familiar_or_not_1_6,axiom,
    ( familiar(n1,n6)
    | not_familiar(n1,n6) )).

%----X=2
%----Y-6
cnf(familiar_or_not_2_1,axiom,
    ( familiar(n2,n1)
    | not_familiar(n2,n1) )).

cnf(familiar_or_not_2_3,axiom,
    ( familiar(n2,n3)
    | not_familiar(n2,n3) )).

cnf(familiar_or_not_2_4,axiom,
    ( familiar(n2,n4)
    | not_familiar(n2,n4) )).

cnf(familiar_or_not_2_5,axiom,
    ( familiar(n2,n5)
    | not_familiar(n2,n5) )).

cnf(familiar_or_not_2_6,axiom,
    ( familiar(n2,n6)
    | not_familiar(n2,n6) )).

%----X=3
%----Y-6
cnf(familiar_or_not_3_1,axiom,
    ( familiar(n3,n1)
    | not_familiar(n3,n1) )).

cnf(familiar_or_not_3_2,axiom,
    ( familiar(n3,n2)
    | not_familiar(n3,n2) )).

cnf(familiar_or_not_3_4,axiom,
    ( familiar(n3,n4)
    | not_familiar(n3,n4) )).

cnf(familiar_or_not_3_5,axiom,
    ( familiar(n3,n5)
    | not_familiar(n3,n5) )).

cnf(familiar_or_not_3_6,axiom,
    ( familiar(n3,n6)
    | not_familiar(n3,n6) )).

%----X=4
%----Y-6
cnf(familiar_or_not_4_1,axiom,
    ( familiar(n4,n1)
    | not_familiar(n4,n1) )).

cnf(familiar_or_not_4_2,axiom,
    ( familiar(n4,n2)
    | not_familiar(n4,n2) )).

cnf(familiar_or_not_4_3,axiom,
    ( familiar(n4,n3)
    | not_familiar(n4,n3) )).

cnf(familiar_or_not_4_5,axiom,
    ( familiar(n4,n5)
    | not_familiar(n4,n5) )).

cnf(familiar_or_not_4_6,axiom,
    ( familiar(n4,n6)
    | not_familiar(n4,n6) )).

%----X=5
%----Y-6
cnf(familiar_or_not_5_1,axiom,
    ( familiar(n5,n1)
    | not_familiar(n5,n1) )).

cnf(familiar_or_not_5_2,axiom,
    ( familiar(n5,n2)
    | not_familiar(n5,n2) )).

cnf(familiar_or_not_5_3,axiom,
    ( familiar(n5,n3)
    | not_familiar(n5,n3) )).

cnf(familiar_or_not_5_4,axiom,
    ( familiar(n5,n4)
    | not_familiar(n5,n4) )).

cnf(familiar_or_not_5_6,axiom,
    ( familiar(n5,n6)
    | not_familiar(n5,n6) )).

%----X=6
%----Y-6
cnf(familiar_or_not_6_1,axiom,
    ( familiar(n6,n1)
    | not_familiar(n6,n1) )).

cnf(familiar_or_not_6_2,axiom,
    ( familiar(n6,n2)
    | not_familiar(n6,n2) )).

cnf(familiar_or_not_6_3,axiom,
    ( familiar(n6,n3)
    | not_familiar(n6,n3) )).

cnf(familiar_or_not_6_4,axiom,
    ( familiar(n6,n4)
    | not_familiar(n6,n4) )).

cnf(familiar_or_not_6_5,axiom,
    ( familiar(n6,n5)
    | not_familiar(n6,n5) )).

%----       --familiar(X1,X2),
%----       --familiar(X2,X3),
%----       --familiar(X3,X1)]).
%----All ground instances of the above clause:
%----X=1
%----Y=1
%----Z-6
%----X=1
%----Y=2
%----Z-6
cnf(three_familiar_1_2_3,negated_conjecture,
    ( ~ familiar(n1,n2)
    | ~ familiar(n2,n3)
    | ~ familiar(n3,n1) )).

cnf(three_familiar_1_2_4,negated_conjecture,
    ( ~ familiar(n1,n2)
    | ~ familiar(n2,n4)
    | ~ familiar(n4,n1) )).

cnf(three_familiar_1_2_5,negated_conjecture,
    ( ~ familiar(n1,n2)
    | ~ familiar(n2,n5)
    | ~ familiar(n5,n1) )).

cnf(three_familiar_1_2_6,negated_conjecture,
    ( ~ familiar(n1,n2)
    | ~ familiar(n2,n6)
    | ~ familiar(n6,n1) )).

%----X=1
%----Y=3
%----Z-6
cnf(three_familiar_1_3_2,negated_conjecture,
    ( ~ familiar(n1,n3)
    | ~ familiar(n3,n2)
    | ~ familiar(n2,n1) )).

cnf(three_familiar_1_3_4,negated_conjecture,
    ( ~ familiar(n1,n3)
    | ~ familiar(n3,n4)
    | ~ familiar(n4,n1) )).

cnf(three_familiar_1_3_5,negated_conjecture,
    ( ~ familiar(n1,n3)
    | ~ familiar(n3,n5)
    | ~ familiar(n5,n1) )).

cnf(three_familiar_1_3_6,negated_conjecture,
    ( ~ familiar(n1,n3)
    | ~ familiar(n3,n6)
    | ~ familiar(n6,n1) )).

%----X=1
%----Y=4
%----Z-6
cnf(three_familiar_1_4_2,negated_conjecture,
    ( ~ familiar(n1,n4)
    | ~ familiar(n4,n2)
    | ~ familiar(n2,n1) )).

cnf(three_familiar_1_4_3,negated_conjecture,
    ( ~ familiar(n1,n4)
    | ~ familiar(n4,n3)
    | ~ familiar(n3,n1) )).

cnf(three_familiar_1_4_5,negated_conjecture,
    ( ~ familiar(n1,n4)
    | ~ familiar(n4,n5)
    | ~ familiar(n5,n1) )).

cnf(three_familiar_1_4_6,negated_conjecture,
    ( ~ familiar(n1,n4)
    | ~ familiar(n4,n6)
    | ~ familiar(n6,n1) )).

%----X=1
%----Y=5
%----Z-6
cnf(three_familiar_1_5_2,negated_conjecture,
    ( ~ familiar(n1,n5)
    | ~ familiar(n5,n2)
    | ~ familiar(n2,n1) )).

cnf(three_familiar_1_5_3,negated_conjecture,
    ( ~ familiar(n1,n5)
    | ~ familiar(n5,n3)
    | ~ familiar(n3,n1) )).

cnf(three_familiar_1_5_4,negated_conjecture,
    ( ~ familiar(n1,n5)
    | ~ familiar(n5,n4)
    | ~ familiar(n4,n1) )).

cnf(three_familiar_1_5_6,negated_conjecture,
    ( ~ familiar(n1,n5)
    | ~ familiar(n5,n6)
    | ~ familiar(n6,n1) )).

%----X=1
%----Y=6
%----Z-6
cnf(three_familiar_1_6_2,negated_conjecture,
    ( ~ familiar(n1,n6)
    | ~ familiar(n6,n2)
    | ~ familiar(n2,n1) )).

cnf(three_familiar_1_6_3,negated_conjecture,
    ( ~ familiar(n1,n6)
    | ~ familiar(n6,n3)
    | ~ familiar(n3,n1) )).

cnf(three_familiar_1_6_4,negated_conjecture,
    ( ~ familiar(n1,n6)
    | ~ familiar(n6,n4)
    | ~ familiar(n4,n1) )).

cnf(three_familiar_1_6_5,negated_conjecture,
    ( ~ familiar(n1,n6)
    | ~ familiar(n6,n5)
    | ~ familiar(n5,n1) )).

%----X=2
%----Y=1
%----Z-6
cnf(three_familiar_2_1_3,negated_conjecture,
    ( ~ familiar(n2,n1)
    | ~ familiar(n1,n3)
    | ~ familiar(n3,n2) )).

cnf(three_familiar_2_1_4,negated_conjecture,
    ( ~ familiar(n2,n1)
    | ~ familiar(n1,n4)
    | ~ familiar(n4,n2) )).

cnf(three_familiar_2_1_5,negated_conjecture,
    ( ~ familiar(n2,n1)
    | ~ familiar(n1,n5)
    | ~ familiar(n5,n2) )).

cnf(three_familiar_2_1_6,negated_conjecture,
    ( ~ familiar(n2,n1)
    | ~ familiar(n1,n6)
    | ~ familiar(n6,n2) )).

%----X=2
%----Y=2
%----Z-6
%----X=2
%----Y=3
%----Z-6
cnf(three_familiar_2_3_1,negated_conjecture,
    ( ~ familiar(n2,n3)
    | ~ familiar(n3,n1)
    | ~ familiar(n1,n2) )).

cnf(three_familiar_2_3_4,negated_conjecture,
    ( ~ familiar(n2,n3)
    | ~ familiar(n3,n4)
    | ~ familiar(n4,n2) )).

cnf(three_familiar_2_3_5,negated_conjecture,
    ( ~ familiar(n2,n3)
    | ~ familiar(n3,n5)
    | ~ familiar(n5,n2) )).

cnf(three_familiar_2_3_6,negated_conjecture,
    ( ~ familiar(n2,n3)
    | ~ familiar(n3,n6)
    | ~ familiar(n6,n2) )).

%----X=2
%----Y=4
%----Z-6
cnf(three_familiar_2_4_1,negated_conjecture,
    ( ~ familiar(n2,n4)
    | ~ familiar(n4,n1)
    | ~ familiar(n1,n2) )).

cnf(three_familiar_2_4_3,negated_conjecture,
    ( ~ familiar(n2,n4)
    | ~ familiar(n4,n3)
    | ~ familiar(n3,n2) )).

cnf(three_familiar_2_4_5,negated_conjecture,
    ( ~ familiar(n2,n4)
    | ~ familiar(n4,n5)
    | ~ familiar(n5,n2) )).

cnf(three_familiar_2_4_6,negated_conjecture,
    ( ~ familiar(n2,n4)
    | ~ familiar(n4,n6)
    | ~ familiar(n6,n2) )).

%----X=2
%----Y=5
%----Z-6
cnf(three_familiar_2_5_1,negated_conjecture,
    ( ~ familiar(n2,n5)
    | ~ familiar(n5,n1)
    | ~ familiar(n1,n2) )).

cnf(three_familiar_2_5_3,negated_conjecture,
    ( ~ familiar(n2,n5)
    | ~ familiar(n5,n3)
    | ~ familiar(n3,n2) )).

cnf(three_familiar_2_5_4,negated_conjecture,
    ( ~ familiar(n2,n5)
    | ~ familiar(n5,n4)
    | ~ familiar(n4,n2) )).

cnf(three_familiar_2_5_6,negated_conjecture,
    ( ~ familiar(n2,n5)
    | ~ familiar(n5,n6)
    | ~ familiar(n6,n2) )).

%----X=2
%----Y=6
%----Z-6
cnf(three_familiar_2_6_1,negated_conjecture,
    ( ~ familiar(n2,n6)
    | ~ familiar(n6,n1)
    | ~ familiar(n1,n2) )).

cnf(three_familiar_2_6_3,negated_conjecture,
    ( ~ familiar(n2,n6)
    | ~ familiar(n6,n3)
    | ~ familiar(n3,n2) )).

cnf(three_familiar_2_6_4,negated_conjecture,
    ( ~ familiar(n2,n6)
    | ~ familiar(n6,n4)
    | ~ familiar(n4,n2) )).

cnf(three_familiar_2_6_5,negated_conjecture,
    ( ~ familiar(n2,n6)
    | ~ familiar(n6,n5)
    | ~ familiar(n5,n2) )).

%----X=3
%----Y=1
%----Z-1
cnf(three_familiar_3_1_2,negated_conjecture,
    ( ~ familiar(n3,n1)
    | ~ familiar(n1,n2)
    | ~ familiar(n2,n3) )).

cnf(three_familiar_3_1_4,negated_conjecture,
    ( ~ familiar(n3,n1)
    | ~ familiar(n1,n4)
    | ~ familiar(n4,n3) )).

cnf(three_familiar_3_1_5,negated_conjecture,
    ( ~ familiar(n3,n1)
    | ~ familiar(n1,n5)
    | ~ familiar(n5,n3) )).

cnf(three_familiar_3_1_6,negated_conjecture,
    ( ~ familiar(n3,n1)
    | ~ familiar(n1,n6)
    | ~ familiar(n6,n3) )).

%----X=3
%----Y=2
%----Z-6
cnf(three_familiar_3_2_1,negated_conjecture,
    ( ~ familiar(n3,n2)
    | ~ familiar(n2,n1)
    | ~ familiar(n1,n3) )).

cnf(three_familiar_3_2_4,negated_conjecture,
    ( ~ familiar(n3,n2)
    | ~ familiar(n2,n4)
    | ~ familiar(n4,n3) )).

cnf(three_familiar_3_2_5,negated_conjecture,
    ( ~ familiar(n3,n2)
    | ~ familiar(n2,n5)
    | ~ familiar(n5,n3) )).

cnf(three_familiar_3_2_6,negated_conjecture,
    ( ~ familiar(n3,n2)
    | ~ familiar(n2,n6)
    | ~ familiar(n6,n3) )).

%----X=3
%----Y=3
%----Z-6
%----X=3
%----Y=4
%----Z-6
cnf(three_familiar_3_4_1,negated_conjecture,
    ( ~ familiar(n3,n4)
    | ~ familiar(n4,n1)
    | ~ familiar(n1,n3) )).

cnf(three_familiar_3_4_2,negated_conjecture,
    ( ~ familiar(n3,n4)
    | ~ familiar(n4,n2)
    | ~ familiar(n2,n3) )).

cnf(three_familiar_3_4_5,negated_conjecture,
    ( ~ familiar(n3,n4)
    | ~ familiar(n4,n5)
    | ~ familiar(n5,n3) )).

cnf(three_familiar_3_4_6,negated_conjecture,
    ( ~ familiar(n3,n4)
    | ~ familiar(n4,n6)
    | ~ familiar(n6,n3) )).

%----X=3
%----Y=5
%----Z-6
cnf(three_familiar_3_5_1,negated_conjecture,
    ( ~ familiar(n3,n5)
    | ~ familiar(n5,n1)
    | ~ familiar(n1,n3) )).

cnf(three_familiar_3_5_2,negated_conjecture,
    ( ~ familiar(n3,n5)
    | ~ familiar(n5,n2)
    | ~ familiar(n2,n3) )).

cnf(three_familiar_3_5_4,negated_conjecture,
    ( ~ familiar(n3,n5)
    | ~ familiar(n5,n4)
    | ~ familiar(n4,n3) )).

cnf(three_familiar_3_5_6,negated_conjecture,
    ( ~ familiar(n3,n5)
    | ~ familiar(n5,n6)
    | ~ familiar(n6,n3) )).

%----X=3
%----Y=6
%----Z-6
cnf(three_familiar_3_6_1,negated_conjecture,
    ( ~ familiar(n3,n6)
    | ~ familiar(n6,n1)
    | ~ familiar(n1,n3) )).

cnf(three_familiar_3_6_2,negated_conjecture,
    ( ~ familiar(n3,n6)
    | ~ familiar(n6,n2)
    | ~ familiar(n2,n3) )).

cnf(three_familiar_3_6_4,negated_conjecture,
    ( ~ familiar(n3,n6)
    | ~ familiar(n6,n4)
    | ~ familiar(n4,n3) )).

cnf(three_familiar_3_6_5,negated_conjecture,
    ( ~ familiar(n3,n6)
    | ~ familiar(n6,n5)
    | ~ familiar(n5,n3) )).

%----X=4
%----Y=1
%----Z-6
cnf(three_familiar_4_1_2,negated_conjecture,
    ( ~ familiar(n4,n1)
    | ~ familiar(n1,n2)
    | ~ familiar(n2,n4) )).

cnf(three_familiar_4_1_3,negated_conjecture,
    ( ~ familiar(n4,n1)
    | ~ familiar(n1,n3)
    | ~ familiar(n3,n4) )).

cnf(three_familiar_4_1_5,negated_conjecture,
    ( ~ familiar(n4,n1)
    | ~ familiar(n1,n5)
    | ~ familiar(n5,n4) )).

cnf(three_familiar_4_1_6,negated_conjecture,
    ( ~ familiar(n4,n1)
    | ~ familiar(n1,n6)
    | ~ familiar(n6,n4) )).

%----X=4
%----Y=2
%----Z-6
cnf(three_familiar_4_2_1,negated_conjecture,
    ( ~ familiar(n4,n2)
    | ~ familiar(n2,n1)
    | ~ familiar(n1,n4) )).

cnf(three_familiar_4_2_3,negated_conjecture,
    ( ~ familiar(n4,n2)
    | ~ familiar(n2,n3)
    | ~ familiar(n3,n4) )).

cnf(three_familiar_4_2_5,negated_conjecture,
    ( ~ familiar(n4,n2)
    | ~ familiar(n2,n5)
    | ~ familiar(n5,n4) )).

cnf(three_familiar_4_2_6,negated_conjecture,
    ( ~ familiar(n4,n2)
    | ~ familiar(n2,n6)
    | ~ familiar(n6,n4) )).

%----X=4
%----Y=3
%----Z-6
cnf(three_familiar_4_3_1,negated_conjecture,
    ( ~ familiar(n4,n3)
    | ~ familiar(n3,n1)
    | ~ familiar(n1,n4) )).

cnf(three_familiar_4_3_2,negated_conjecture,
    ( ~ familiar(n4,n3)
    | ~ familiar(n3,n2)
    | ~ familiar(n2,n4) )).

cnf(three_familiar_4_3_5,negated_conjecture,
    ( ~ familiar(n4,n3)
    | ~ familiar(n3,n5)
    | ~ familiar(n5,n4) )).

cnf(three_familiar_4_3_6,negated_conjecture,
    ( ~ familiar(n4,n3)
    | ~ familiar(n3,n6)
    | ~ familiar(n6,n4) )).

%----X=4
%----Y=4
%----Z-6
%----X=4
%----Y=5
%----Z-6
cnf(three_familiar_4_5_1,negated_conjecture,
    ( ~ familiar(n4,n5)
    | ~ familiar(n5,n1)
    | ~ familiar(n1,n4) )).

cnf(three_familiar_4_5_2,negated_conjecture,
    ( ~ familiar(n4,n5)
    | ~ familiar(n5,n2)
    | ~ familiar(n2,n4) )).

cnf(three_familiar_4_5_3,negated_conjecture,
    ( ~ familiar(n4,n5)
    | ~ familiar(n5,n3)
    | ~ familiar(n3,n4) )).

cnf(three_familiar_4_5_6,negated_conjecture,
    ( ~ familiar(n4,n5)
    | ~ familiar(n5,n6)
    | ~ familiar(n6,n4) )).

%----X=4
%----Y=6
%----Z-6
cnf(three_familiar_4_6_1,negated_conjecture,
    ( ~ familiar(n4,n6)
    | ~ familiar(n6,n1)
    | ~ familiar(n1,n4) )).

cnf(three_familiar_4_6_2,negated_conjecture,
    ( ~ familiar(n4,n6)
    | ~ familiar(n6,n2)
    | ~ familiar(n2,n4) )).

cnf(three_familiar_4_6_3,negated_conjecture,
    ( ~ familiar(n4,n6)
    | ~ familiar(n6,n3)
    | ~ familiar(n3,n4) )).

cnf(three_familiar_4_6_5,negated_conjecture,
    ( ~ familiar(n4,n6)
    | ~ familiar(n6,n5)
    | ~ familiar(n5,n4) )).

%----X=5
%----Y=1
%----Z-6
cnf(three_familiar_5_1_2,negated_conjecture,
    ( ~ familiar(n5,n1)
    | ~ familiar(n1,n2)
    | ~ familiar(n2,n5) )).

cnf(three_familiar_5_1_3,negated_conjecture,
    ( ~ familiar(n5,n1)
    | ~ familiar(n1,n3)
    | ~ familiar(n3,n5) )).

cnf(three_familiar_5_1_4,negated_conjecture,
    ( ~ familiar(n5,n1)
    | ~ familiar(n1,n4)
    | ~ familiar(n4,n5) )).

cnf(three_familiar_5_1_6,negated_conjecture,
    ( ~ familiar(n5,n1)
    | ~ familiar(n1,n6)
    | ~ familiar(n6,n5) )).

%----X=5
%----Y=2
%----Z-6
cnf(three_familiar_5_2_1,negated_conjecture,
    ( ~ familiar(n5,n2)
    | ~ familiar(n2,n1)
    | ~ familiar(n1,n5) )).

cnf(three_familiar_5_2_3,negated_conjecture,
    ( ~ familiar(n5,n2)
    | ~ familiar(n2,n3)
    | ~ familiar(n3,n5) )).

cnf(three_familiar_5_2_4,negated_conjecture,
    ( ~ familiar(n5,n2)
    | ~ familiar(n2,n4)
    | ~ familiar(n4,n5) )).

cnf(three_familiar_5_2_6,negated_conjecture,
    ( ~ familiar(n5,n2)
    | ~ familiar(n2,n6)
    | ~ familiar(n6,n5) )).

%----X=5
%----Y=3
%----Z-6
cnf(three_familiar_5_3_1,negated_conjecture,
    ( ~ familiar(n5,n3)
    | ~ familiar(n3,n1)
    | ~ familiar(n1,n5) )).

cnf(three_familiar_5_3_2,negated_conjecture,
    ( ~ familiar(n5,n3)
    | ~ familiar(n3,n2)
    | ~ familiar(n2,n5) )).

cnf(three_familiar_5_3_4,negated_conjecture,
    ( ~ familiar(n5,n3)
    | ~ familiar(n3,n4)
    | ~ familiar(n4,n5) )).

cnf(three_familiar_5_3_6,negated_conjecture,
    ( ~ familiar(n5,n3)
    | ~ familiar(n3,n6)
    | ~ familiar(n6,n5) )).

%----X=5
%----Y=4
%----Z-6
cnf(three_familiar_5_4_1,negated_conjecture,
    ( ~ familiar(n5,n4)
    | ~ familiar(n4,n1)
    | ~ familiar(n1,n5) )).

cnf(three_familiar_5_4_2,negated_conjecture,
    ( ~ familiar(n5,n4)
    | ~ familiar(n4,n2)
    | ~ familiar(n2,n5) )).

cnf(three_familiar_5_4_3,negated_conjecture,
    ( ~ familiar(n5,n4)
    | ~ familiar(n4,n3)
    | ~ familiar(n3,n5) )).

cnf(three_familiar_5_4_6,negated_conjecture,
    ( ~ familiar(n5,n4)
    | ~ familiar(n4,n6)
    | ~ familiar(n6,n5) )).

%----X=5
%----Y=5
%----Z-6
%----X=5
%----Y=6
%----Z-6
cnf(three_familiar_5_6_1,negated_conjecture,
    ( ~ familiar(n5,n6)
    | ~ familiar(n6,n1)
    | ~ familiar(n1,n5) )).

cnf(three_familiar_5_6_2,negated_conjecture,
    ( ~ familiar(n5,n6)
    | ~ familiar(n6,n2)
    | ~ familiar(n2,n5) )).

cnf(three_familiar_5_6_3,negated_conjecture,
    ( ~ familiar(n5,n6)
    | ~ familiar(n6,n3)
    | ~ familiar(n3,n5) )).

cnf(three_familiar_5_6_4,negated_conjecture,
    ( ~ familiar(n5,n6)
    | ~ familiar(n6,n4)
    | ~ familiar(n4,n5) )).

%----X=6
%----Y=1
%----Z-6
cnf(three_familiar_6_1_2,negated_conjecture,
    ( ~ familiar(n6,n1)
    | ~ familiar(n1,n2)
    | ~ familiar(n2,n6) )).

cnf(three_familiar_6_1_3,negated_conjecture,
    ( ~ familiar(n6,n1)
    | ~ familiar(n1,n3)
    | ~ familiar(n3,n6) )).

cnf(three_familiar_6_1_4,negated_conjecture,
    ( ~ familiar(n6,n1)
    | ~ familiar(n1,n4)
    | ~ familiar(n4,n6) )).

cnf(three_familiar_6_1_5,negated_conjecture,
    ( ~ familiar(n6,n1)
    | ~ familiar(n1,n5)
    | ~ familiar(n5,n6) )).

%----X=6
%----Y=2
%----Z-6
cnf(three_familiar_6_2_1,negated_conjecture,
    ( ~ familiar(n6,n2)
    | ~ familiar(n2,n1)
    | ~ familiar(n1,n6) )).

cnf(three_familiar_6_2_3,negated_conjecture,
    ( ~ familiar(n6,n2)
    | ~ familiar(n2,n3)
    | ~ familiar(n3,n6) )).

cnf(three_familiar_6_2_4,negated_conjecture,
    ( ~ familiar(n6,n2)
    | ~ familiar(n2,n4)
    | ~ familiar(n4,n6) )).

cnf(three_familiar_6_2_5,negated_conjecture,
    ( ~ familiar(n6,n2)
    | ~ familiar(n2,n5)
    | ~ familiar(n5,n6) )).

%----X=6
%----Y=3
%----Z-6
cnf(three_familiar_6_3_1,negated_conjecture,
    ( ~ familiar(n6,n3)
    | ~ familiar(n3,n1)
    | ~ familiar(n1,n6) )).

cnf(three_familiar_6_3_2,negated_conjecture,
    ( ~ familiar(n6,n3)
    | ~ familiar(n3,n2)
    | ~ familiar(n2,n6) )).

cnf(three_familiar_6_3_4,negated_conjecture,
    ( ~ familiar(n6,n3)
    | ~ familiar(n3,n4)
    | ~ familiar(n4,n6) )).

cnf(three_familiar_6_3_5,negated_conjecture,
    ( ~ familiar(n6,n3)
    | ~ familiar(n3,n5)
    | ~ familiar(n5,n6) )).

%----X=6
%----Y=4
%----Z-6
cnf(three_familiar_6_4_1,negated_conjecture,
    ( ~ familiar(n6,n4)
    | ~ familiar(n4,n1)
    | ~ familiar(n1,n6) )).

cnf(three_familiar_6_4_2,negated_conjecture,
    ( ~ familiar(n6,n4)
    | ~ familiar(n4,n2)
    | ~ familiar(n2,n6) )).

cnf(three_familiar_6_4_3,negated_conjecture,
    ( ~ familiar(n6,n4)
    | ~ familiar(n4,n3)
    | ~ familiar(n3,n6) )).

cnf(three_familiar_6_4_5,negated_conjecture,
    ( ~ familiar(n6,n4)
    | ~ familiar(n4,n5)
    | ~ familiar(n5,n6) )).

%----X=6
%----Y=5
%----Z-6
cnf(three_familiar_6_5_1,negated_conjecture,
    ( ~ familiar(n6,n5)
    | ~ familiar(n5,n1)
    | ~ familiar(n1,n6) )).

cnf(three_familiar_6_5_2,negated_conjecture,
    ( ~ familiar(n6,n5)
    | ~ familiar(n5,n2)
    | ~ familiar(n2,n6) )).

cnf(three_familiar_6_5_3,negated_conjecture,
    ( ~ familiar(n6,n5)
    | ~ familiar(n5,n3)
    | ~ familiar(n3,n6) )).

cnf(three_familiar_6_5_4,negated_conjecture,
    ( ~ familiar(n6,n5)
    | ~ familiar(n5,n4)
    | ~ familiar(n4,n6) )).

%----X=6
%----Y=6
%----Z-6
%----     --not_familiar(X1,X2),
%----     --not_familiar(X2,X3),
%----     --not_familiar(X3,X1)]).
%----All ground instances of the above clause:
%----X=1
%----Y=1
%----Z-6
%----X=1
%----Y=2
%----Z-6
cnf(three_not_familiar_1_2_3,negated_conjecture,
    ( ~ not_familiar(n1,n2)
    | ~ not_familiar(n2,n3)
    | ~ not_familiar(n3,n1) )).

cnf(three_not_familiar_1_2_4,negated_conjecture,
    ( ~ not_familiar(n1,n2)
    | ~ not_familiar(n2,n4)
    | ~ not_familiar(n4,n1) )).

cnf(three_not_familiar_1_2_5,negated_conjecture,
    ( ~ not_familiar(n1,n2)
    | ~ not_familiar(n2,n5)
    | ~ not_familiar(n5,n1) )).

cnf(three_not_familiar_1_2_6,negated_conjecture,
    ( ~ not_familiar(n1,n2)
    | ~ not_familiar(n2,n6)
    | ~ not_familiar(n6,n1) )).

%----X=1
%----Y=3
%----Z-6
cnf(three_not_familiar_1_3_2,negated_conjecture,
    ( ~ not_familiar(n1,n3)
    | ~ not_familiar(n3,n2)
    | ~ not_familiar(n2,n1) )).

cnf(three_not_familiar_1_3_4,negated_conjecture,
    ( ~ not_familiar(n1,n3)
    | ~ not_familiar(n3,n4)
    | ~ not_familiar(n4,n1) )).

cnf(three_not_familiar_1_3_5,negated_conjecture,
    ( ~ not_familiar(n1,n3)
    | ~ not_familiar(n3,n5)
    | ~ not_familiar(n5,n1) )).

cnf(three_not_familiar_1_3_6,negated_conjecture,
    ( ~ not_familiar(n1,n3)
    | ~ not_familiar(n3,n6)
    | ~ not_familiar(n6,n1) )).

%----X=1
%----Y=4
%----Z-6
cnf(three_not_familiar_1_4_2,negated_conjecture,
    ( ~ not_familiar(n1,n4)
    | ~ not_familiar(n4,n2)
    | ~ not_familiar(n2,n1) )).

cnf(three_not_familiar_1_4_3,negated_conjecture,
    ( ~ not_familiar(n1,n4)
    | ~ not_familiar(n4,n3)
    | ~ not_familiar(n3,n1) )).

cnf(three_not_familiar_1_4_5,negated_conjecture,
    ( ~ not_familiar(n1,n4)
    | ~ not_familiar(n4,n5)
    | ~ not_familiar(n5,n1) )).

cnf(three_not_familiar_1_4_6,negated_conjecture,
    ( ~ not_familiar(n1,n4)
    | ~ not_familiar(n4,n6)
    | ~ not_familiar(n6,n1) )).

%----X=1
%----Y=5
%----Z-6
cnf(three_not_familiar_1_5_2,negated_conjecture,
    ( ~ not_familiar(n1,n5)
    | ~ not_familiar(n5,n2)
    | ~ not_familiar(n2,n1) )).

cnf(three_not_familiar_1_5_3,negated_conjecture,
    ( ~ not_familiar(n1,n5)
    | ~ not_familiar(n5,n3)
    | ~ not_familiar(n3,n1) )).

cnf(three_not_familiar_1_5_4,negated_conjecture,
    ( ~ not_familiar(n1,n5)
    | ~ not_familiar(n5,n4)
    | ~ not_familiar(n4,n1) )).

cnf(three_not_familiar_1_5_6,negated_conjecture,
    ( ~ not_familiar(n1,n5)
    | ~ not_familiar(n5,n6)
    | ~ not_familiar(n6,n1) )).

%----X=1
%----Y=6
%----Z-6
cnf(three_not_familiar_1_6_2,negated_conjecture,
    ( ~ not_familiar(n1,n6)
    | ~ not_familiar(n6,n2)
    | ~ not_familiar(n2,n1) )).

cnf(three_not_familiar_1_6_3,negated_conjecture,
    ( ~ not_familiar(n1,n6)
    | ~ not_familiar(n6,n3)
    | ~ not_familiar(n3,n1) )).

cnf(three_not_familiar_1_6_4,negated_conjecture,
    ( ~ not_familiar(n1,n6)
    | ~ not_familiar(n6,n4)
    | ~ not_familiar(n4,n1) )).

cnf(three_not_familiar_1_6_5,negated_conjecture,
    ( ~ not_familiar(n1,n6)
    | ~ not_familiar(n6,n5)
    | ~ not_familiar(n5,n1) )).

%----X=2
%----Y=1
%----Z-6
cnf(three_not_familiar_2_1_3,negated_conjecture,
    ( ~ not_familiar(n2,n1)
    | ~ not_familiar(n1,n3)
    | ~ not_familiar(n3,n2) )).

cnf(three_not_familiar_2_1_4,negated_conjecture,
    ( ~ not_familiar(n2,n1)
    | ~ not_familiar(n1,n4)
    | ~ not_familiar(n4,n2) )).

cnf(three_not_familiar_2_1_5,negated_conjecture,
    ( ~ not_familiar(n2,n1)
    | ~ not_familiar(n1,n5)
    | ~ not_familiar(n5,n2) )).

cnf(three_not_familiar_2_1_6,negated_conjecture,
    ( ~ not_familiar(n2,n1)
    | ~ not_familiar(n1,n6)
    | ~ not_familiar(n6,n2) )).

%----X=2
%----Y=2
%----Z-6
%----X=2
%----Y=3
%----Z-6
cnf(three_not_familiar_2_3_1,negated_conjecture,
    ( ~ not_familiar(n2,n3)
    | ~ not_familiar(n3,n1)
    | ~ not_familiar(n1,n2) )).

cnf(three_not_familiar_2_3_4,negated_conjecture,
    ( ~ not_familiar(n2,n3)
    | ~ not_familiar(n3,n4)
    | ~ not_familiar(n4,n2) )).

cnf(three_not_familiar_2_3_5,negated_conjecture,
    ( ~ not_familiar(n2,n3)
    | ~ not_familiar(n3,n5)
    | ~ not_familiar(n5,n2) )).

cnf(three_not_familiar_2_3_6,negated_conjecture,
    ( ~ not_familiar(n2,n3)
    | ~ not_familiar(n3,n6)
    | ~ not_familiar(n6,n2) )).

%----X=2
%----Y=4
%----Z-6
cnf(three_not_familiar_2_4_1,negated_conjecture,
    ( ~ not_familiar(n2,n4)
    | ~ not_familiar(n4,n1)
    | ~ not_familiar(n1,n2) )).

cnf(three_not_familiar_2_4_3,negated_conjecture,
    ( ~ not_familiar(n2,n4)
    | ~ not_familiar(n4,n3)
    | ~ not_familiar(n3,n2) )).

cnf(three_not_familiar_2_4_5,negated_conjecture,
    ( ~ not_familiar(n2,n4)
    | ~ not_familiar(n4,n5)
    | ~ not_familiar(n5,n2) )).

cnf(three_not_familiar_2_4_6,negated_conjecture,
    ( ~ not_familiar(n2,n4)
    | ~ not_familiar(n4,n6)
    | ~ not_familiar(n6,n2) )).

%----X=2
%----Y=5
%----Z-6
cnf(three_not_familiar_2_5_1,negated_conjecture,
    ( ~ not_familiar(n2,n5)
    | ~ not_familiar(n5,n1)
    | ~ not_familiar(n1,n2) )).

cnf(three_not_familiar_2_5_3,negated_conjecture,
    ( ~ not_familiar(n2,n5)
    | ~ not_familiar(n5,n3)
    | ~ not_familiar(n3,n2) )).

cnf(three_not_familiar_2_5_4,negated_conjecture,
    ( ~ not_familiar(n2,n5)
    | ~ not_familiar(n5,n4)
    | ~ not_familiar(n4,n2) )).

cnf(three_not_familiar_2_5_6,negated_conjecture,
    ( ~ not_familiar(n2,n5)
    | ~ not_familiar(n5,n6)
    | ~ not_familiar(n6,n2) )).

%----X=2
%----Y=6
%----Z-6
cnf(three_not_familiar_2_6_1,negated_conjecture,
    ( ~ not_familiar(n2,n6)
    | ~ not_familiar(n6,n1)
    | ~ not_familiar(n1,n2) )).

cnf(three_not_familiar_2_6_3,negated_conjecture,
    ( ~ not_familiar(n2,n6)
    | ~ not_familiar(n6,n3)
    | ~ not_familiar(n3,n2) )).

cnf(three_not_familiar_2_6_4,negated_conjecture,
    ( ~ not_familiar(n2,n6)
    | ~ not_familiar(n6,n4)
    | ~ not_familiar(n4,n2) )).

cnf(three_not_familiar_2_6_5,negated_conjecture,
    ( ~ not_familiar(n2,n6)
    | ~ not_familiar(n6,n5)
    | ~ not_familiar(n5,n2) )).

%----X=3
%----Y=1
%----Z-1
cnf(three_not_familiar_3_1_2,negated_conjecture,
    ( ~ not_familiar(n3,n1)
    | ~ not_familiar(n1,n2)
    | ~ not_familiar(n2,n3) )).

cnf(three_not_familiar_3_1_4,negated_conjecture,
    ( ~ not_familiar(n3,n1)
    | ~ not_familiar(n1,n4)
    | ~ not_familiar(n4,n3) )).

cnf(three_not_familiar_3_1_5,negated_conjecture,
    ( ~ not_familiar(n3,n1)
    | ~ not_familiar(n1,n5)
    | ~ not_familiar(n5,n3) )).

cnf(three_not_familiar_3_1_6,negated_conjecture,
    ( ~ not_familiar(n3,n1)
    | ~ not_familiar(n1,n6)
    | ~ not_familiar(n6,n3) )).

%----X=3
%----Y=2
%----Z-6
cnf(three_not_familiar_3_2_1,negated_conjecture,
    ( ~ not_familiar(n3,n2)
    | ~ not_familiar(n2,n1)
    | ~ not_familiar(n1,n3) )).

cnf(three_not_familiar_3_2_4,negated_conjecture,
    ( ~ not_familiar(n3,n2)
    | ~ not_familiar(n2,n4)
    | ~ not_familiar(n4,n3) )).

cnf(three_not_familiar_3_2_5,negated_conjecture,
    ( ~ not_familiar(n3,n2)
    | ~ not_familiar(n2,n5)
    | ~ not_familiar(n5,n3) )).

cnf(three_not_familiar_3_2_6,negated_conjecture,
    ( ~ not_familiar(n3,n2)
    | ~ not_familiar(n2,n6)
    | ~ not_familiar(n6,n3) )).

%----X=3
%----Y=3
%----Z-6
%----X=3
%----Y=4
%----Z-6
cnf(three_not_familiar_3_4_1,negated_conjecture,
    ( ~ not_familiar(n3,n4)
    | ~ not_familiar(n4,n1)
    | ~ not_familiar(n1,n3) )).

cnf(three_not_familiar_3_4_2,negated_conjecture,
    ( ~ not_familiar(n3,n4)
    | ~ not_familiar(n4,n2)
    | ~ not_familiar(n2,n3) )).

cnf(three_not_familiar_3_4_5,negated_conjecture,
    ( ~ not_familiar(n3,n4)
    | ~ not_familiar(n4,n5)
    | ~ not_familiar(n5,n3) )).

cnf(three_not_familiar_3_4_6,negated_conjecture,
    ( ~ not_familiar(n3,n4)
    | ~ not_familiar(n4,n6)
    | ~ not_familiar(n6,n3) )).

%----X=3
%----Y=5
%----Z-6
cnf(three_not_familiar_3_5_1,negated_conjecture,
    ( ~ not_familiar(n3,n5)
    | ~ not_familiar(n5,n1)
    | ~ not_familiar(n1,n3) )).

cnf(three_not_familiar_3_5_2,negated_conjecture,
    ( ~ not_familiar(n3,n5)
    | ~ not_familiar(n5,n2)
    | ~ not_familiar(n2,n3) )).

cnf(three_not_familiar_3_5_4,negated_conjecture,
    ( ~ not_familiar(n3,n5)
    | ~ not_familiar(n5,n4)
    | ~ not_familiar(n4,n3) )).

cnf(three_not_familiar_3_5_6,negated_conjecture,
    ( ~ not_familiar(n3,n5)
    | ~ not_familiar(n5,n6)
    | ~ not_familiar(n6,n3) )).

%----X=3
%----Y=6
%----Z-6
cnf(three_not_familiar_3_6_1,negated_conjecture,
    ( ~ not_familiar(n3,n6)
    | ~ not_familiar(n6,n1)
    | ~ not_familiar(n1,n3) )).

cnf(three_not_familiar_3_6_2,negated_conjecture,
    ( ~ not_familiar(n3,n6)
    | ~ not_familiar(n6,n2)
    | ~ not_familiar(n2,n3) )).

cnf(three_not_familiar_3_6_4,negated_conjecture,
    ( ~ not_familiar(n3,n6)
    | ~ not_familiar(n6,n4)
    | ~ not_familiar(n4,n3) )).

cnf(three_not_familiar_3_6_5,negated_conjecture,
    ( ~ not_familiar(n3,n6)
    | ~ not_familiar(n6,n5)
    | ~ not_familiar(n5,n3) )).

%----X=4
%----Y=1
%----Z-6
cnf(three_not_familiar_4_1_2,negated_conjecture,
    ( ~ not_familiar(n4,n1)
    | ~ not_familiar(n1,n2)
    | ~ not_familiar(n2,n4) )).

cnf(three_not_familiar_4_1_3,negated_conjecture,
    ( ~ not_familiar(n4,n1)
    | ~ not_familiar(n1,n3)
    | ~ not_familiar(n3,n4) )).

cnf(three_not_familiar_4_1_5,negated_conjecture,
    ( ~ not_familiar(n4,n1)
    | ~ not_familiar(n1,n5)
    | ~ not_familiar(n5,n4) )).

cnf(three_not_familiar_4_1_6,negated_conjecture,
    ( ~ not_familiar(n4,n1)
    | ~ not_familiar(n1,n6)
    | ~ not_familiar(n6,n4) )).

%----X=4
%----Y=2
%----Z-6
cnf(three_not_familiar_4_2_1,negated_conjecture,
    ( ~ not_familiar(n4,n2)
    | ~ not_familiar(n2,n1)
    | ~ not_familiar(n1,n4) )).

cnf(three_not_familiar_4_2_3,negated_conjecture,
    ( ~ not_familiar(n4,n2)
    | ~ not_familiar(n2,n3)
    | ~ not_familiar(n3,n4) )).

cnf(three_not_familiar_4_2_5,negated_conjecture,
    ( ~ not_familiar(n4,n2)
    | ~ not_familiar(n2,n5)
    | ~ not_familiar(n5,n4) )).

cnf(three_not_familiar_4_2_6,negated_conjecture,
    ( ~ not_familiar(n4,n2)
    | ~ not_familiar(n2,n6)
    | ~ not_familiar(n6,n4) )).

%----X=4
%----Y=3
%----Z-6
cnf(three_not_familiar_4_3_1,negated_conjecture,
    ( ~ not_familiar(n4,n3)
    | ~ not_familiar(n3,n1)
    | ~ not_familiar(n1,n4) )).

cnf(three_not_familiar_4_3_2,negated_conjecture,
    ( ~ not_familiar(n4,n3)
    | ~ not_familiar(n3,n2)
    | ~ not_familiar(n2,n4) )).

cnf(three_not_familiar_4_3_5,negated_conjecture,
    ( ~ not_familiar(n4,n3)
    | ~ not_familiar(n3,n5)
    | ~ not_familiar(n5,n4) )).

cnf(three_not_familiar_4_3_6,negated_conjecture,
    ( ~ not_familiar(n4,n3)
    | ~ not_familiar(n3,n6)
    | ~ not_familiar(n6,n4) )).

%----X=4
%----Y=4
%----Z-6
%----X=4
%----Y=5
%----Z-6
cnf(three_not_familiar_4_5_1,negated_conjecture,
    ( ~ not_familiar(n4,n5)
    | ~ not_familiar(n5,n1)
    | ~ not_familiar(n1,n4) )).

cnf(three_not_familiar_4_5_2,negated_conjecture,
    ( ~ not_familiar(n4,n5)
    | ~ not_familiar(n5,n2)
    | ~ not_familiar(n2,n4) )).

cnf(three_not_familiar_4_5_3,negated_conjecture,
    ( ~ not_familiar(n4,n5)
    | ~ not_familiar(n5,n3)
    | ~ not_familiar(n3,n4) )).

cnf(three_not_familiar_4_5_6,negated_conjecture,
    ( ~ not_familiar(n4,n5)
    | ~ not_familiar(n5,n6)
    | ~ not_familiar(n6,n4) )).

%----X=4
%----Y=6
%----Z-6
cnf(three_not_familiar_4_6_1,negated_conjecture,
    ( ~ not_familiar(n4,n6)
    | ~ not_familiar(n6,n1)
    | ~ not_familiar(n1,n4) )).

cnf(three_not_familiar_4_6_2,negated_conjecture,
    ( ~ not_familiar(n4,n6)
    | ~ not_familiar(n6,n2)
    | ~ not_familiar(n2,n4) )).

cnf(three_not_familiar_4_6_3,negated_conjecture,
    ( ~ not_familiar(n4,n6)
    | ~ not_familiar(n6,n3)
    | ~ not_familiar(n3,n4) )).

cnf(three_not_familiar_4_6_5,negated_conjecture,
    ( ~ not_familiar(n4,n6)
    | ~ not_familiar(n6,n5)
    | ~ not_familiar(n5,n4) )).

%----X=5
%----Y=1
%----Z-6
cnf(three_not_familiar_5_1_2,negated_conjecture,
    ( ~ not_familiar(n5,n1)
    | ~ not_familiar(n1,n2)
    | ~ not_familiar(n2,n5) )).

cnf(three_not_familiar_5_1_3,negated_conjecture,
    ( ~ not_familiar(n5,n1)
    | ~ not_familiar(n1,n3)
    | ~ not_familiar(n3,n5) )).

cnf(three_not_familiar_5_1_4,negated_conjecture,
    ( ~ not_familiar(n5,n1)
    | ~ not_familiar(n1,n4)
    | ~ not_familiar(n4,n5) )).

cnf(three_not_familiar_5_1_6,negated_conjecture,
    ( ~ not_familiar(n5,n1)
    | ~ not_familiar(n1,n6)
    | ~ not_familiar(n6,n5) )).

%----X=5
%----Y=2
%----Z-6
cnf(three_not_familiar_5_2_1,negated_conjecture,
    ( ~ not_familiar(n5,n2)
    | ~ not_familiar(n2,n1)
    | ~ not_familiar(n1,n5) )).

cnf(three_not_familiar_5_2_3,negated_conjecture,
    ( ~ not_familiar(n5,n2)
    | ~ not_familiar(n2,n3)
    | ~ not_familiar(n3,n5) )).

cnf(three_not_familiar_5_2_4,negated_conjecture,
    ( ~ not_familiar(n5,n2)
    | ~ not_familiar(n2,n4)
    | ~ not_familiar(n4,n5) )).

cnf(three_not_familiar_5_2_6,negated_conjecture,
    ( ~ not_familiar(n5,n2)
    | ~ not_familiar(n2,n6)
    | ~ not_familiar(n6,n5) )).

%----X=5
%----Y=3
%----Z-6
cnf(three_not_familiar_5_3_1,negated_conjecture,
    ( ~ not_familiar(n5,n3)
    | ~ not_familiar(n3,n1)
    | ~ not_familiar(n1,n5) )).

cnf(three_not_familiar_5_3_2,negated_conjecture,
    ( ~ not_familiar(n5,n3)
    | ~ not_familiar(n3,n2)
    | ~ not_familiar(n2,n5) )).

cnf(three_not_familiar_5_3_4,negated_conjecture,
    ( ~ not_familiar(n5,n3)
    | ~ not_familiar(n3,n4)
    | ~ not_familiar(n4,n5) )).

cnf(three_not_familiar_5_3_6,negated_conjecture,
    ( ~ not_familiar(n5,n3)
    | ~ not_familiar(n3,n6)
    | ~ not_familiar(n6,n5) )).

%----X=5
%----Y=4
%----Z-6
cnf(three_not_familiar_5_4_1,negated_conjecture,
    ( ~ not_familiar(n5,n4)
    | ~ not_familiar(n4,n1)
    | ~ not_familiar(n1,n5) )).

cnf(three_not_familiar_5_4_2,negated_conjecture,
    ( ~ not_familiar(n5,n4)
    | ~ not_familiar(n4,n2)
    | ~ not_familiar(n2,n5) )).

cnf(three_not_familiar_5_4_3,negated_conjecture,
    ( ~ not_familiar(n5,n4)
    | ~ not_familiar(n4,n3)
    | ~ not_familiar(n3,n5) )).

cnf(three_not_familiar_5_4_6,negated_conjecture,
    ( ~ not_familiar(n5,n4)
    | ~ not_familiar(n4,n6)
    | ~ not_familiar(n6,n5) )).

%----X=5
%----Y=5
%----Z-6
%----X=5
%----Y=6
%----Z-6
cnf(three_not_familiar_5_6_1,negated_conjecture,
    ( ~ not_familiar(n5,n6)
    | ~ not_familiar(n6,n1)
    | ~ not_familiar(n1,n5) )).

cnf(three_not_familiar_5_6_2,negated_conjecture,
    ( ~ not_familiar(n5,n6)
    | ~ not_familiar(n6,n2)
    | ~ not_familiar(n2,n5) )).

cnf(three_not_familiar_5_6_3,negated_conjecture,
    ( ~ not_familiar(n5,n6)
    | ~ not_familiar(n6,n3)
    | ~ not_familiar(n3,n5) )).

cnf(three_not_familiar_5_6_4,negated_conjecture,
    ( ~ not_familiar(n5,n6)
    | ~ not_familiar(n6,n4)
    | ~ not_familiar(n4,n5) )).

%----X=6
%----Y=1
%----Z-6
cnf(three_not_familiar_6_1_2,negated_conjecture,
    ( ~ not_familiar(n6,n1)
    | ~ not_familiar(n1,n2)
    | ~ not_familiar(n2,n6) )).

cnf(three_not_familiar_6_1_3,negated_conjecture,
    ( ~ not_familiar(n6,n1)
    | ~ not_familiar(n1,n3)
    | ~ not_familiar(n3,n6) )).

cnf(three_not_familiar_6_1_4,negated_conjecture,
    ( ~ not_familiar(n6,n1)
    | ~ not_familiar(n1,n4)
    | ~ not_familiar(n4,n6) )).

cnf(three_not_familiar_6_1_5,negated_conjecture,
    ( ~ not_familiar(n6,n1)
    | ~ not_familiar(n1,n5)
    | ~ not_familiar(n5,n6) )).

%----X=6
%----Y=2
%----Z-6
cnf(three_not_familiar_6_2_1,negated_conjecture,
    ( ~ not_familiar(n6,n2)
    | ~ not_familiar(n2,n1)
    | ~ not_familiar(n1,n6) )).

cnf(three_not_familiar_6_2_3,negated_conjecture,
    ( ~ not_familiar(n6,n2)
    | ~ not_familiar(n2,n3)
    | ~ not_familiar(n3,n6) )).

cnf(three_not_familiar_6_2_4,negated_conjecture,
    ( ~ not_familiar(n6,n2)
    | ~ not_familiar(n2,n4)
    | ~ not_familiar(n4,n6) )).

cnf(three_not_familiar_6_2_5,negated_conjecture,
    ( ~ not_familiar(n6,n2)
    | ~ not_familiar(n2,n5)
    | ~ not_familiar(n5,n6) )).

%----X=6
%----Y=3
%----Z-6
cnf(three_not_familiar_6_3_1,negated_conjecture,
    ( ~ not_familiar(n6,n3)
    | ~ not_familiar(n3,n1)
    | ~ not_familiar(n1,n6) )).

cnf(three_not_familiar_6_3_2,negated_conjecture,
    ( ~ not_familiar(n6,n3)
    | ~ not_familiar(n3,n2)
    | ~ not_familiar(n2,n6) )).

cnf(three_not_familiar_6_3_4,negated_conjecture,
    ( ~ not_familiar(n6,n3)
    | ~ not_familiar(n3,n4)
    | ~ not_familiar(n4,n6) )).

cnf(three_not_familiar_6_3_5,negated_conjecture,
    ( ~ not_familiar(n6,n3)
    | ~ not_familiar(n3,n5)
    | ~ not_familiar(n5,n6) )).

%----X=6
%----Y=4
%----Z-6
cnf(three_not_familiar_6_4_1,negated_conjecture,
    ( ~ not_familiar(n6,n4)
    | ~ not_familiar(n4,n1)
    | ~ not_familiar(n1,n6) )).

cnf(three_not_familiar_6_4_2,negated_conjecture,
    ( ~ not_familiar(n6,n4)
    | ~ not_familiar(n4,n2)
    | ~ not_familiar(n2,n6) )).

cnf(three_not_familiar_6_4_3,negated_conjecture,
    ( ~ not_familiar(n6,n4)
    | ~ not_familiar(n4,n3)
    | ~ not_familiar(n3,n6) )).

cnf(three_not_familiar_6_4_5,negated_conjecture,
    ( ~ not_familiar(n6,n4)
    | ~ not_familiar(n4,n5)
    | ~ not_familiar(n5,n6) )).

%----X=6
%----Y=5
%----Z-6
cnf(three_not_familiar_6_5_1,negated_conjecture,
    ( ~ not_familiar(n6,n5)
    | ~ not_familiar(n5,n1)
    | ~ not_familiar(n1,n6) )).

cnf(three_not_familiar_6_5_2,negated_conjecture,
    ( ~ not_familiar(n6,n5)
    | ~ not_familiar(n5,n2)
    | ~ not_familiar(n2,n6) )).

cnf(three_not_familiar_6_5_3,negated_conjecture,
    ( ~ not_familiar(n6,n5)
    | ~ not_familiar(n5,n3)
    | ~ not_familiar(n3,n6) )).

cnf(three_not_familiar_6_5_4,negated_conjecture,
    ( ~ not_familiar(n6,n5)
    | ~ not_familiar(n5,n4)
    | ~ not_familiar(n4,n6) )).

%----X=6
%----Y=6
%----Z-6
%--------------------------------------------------------------------------
