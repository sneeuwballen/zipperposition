%--------------------------------------------------------------------------
% File     : PUZ047-1 : TPTP v5.2.0. Released v2.5.0.
% Domain   : Syntactic
% Problem  : Taking the wolf, goat, and cabbage across river
% Version  : Especial.
% English  :

% Refs     : [And97] Andrews (1994), Email to G. Sutcliffe
% Source   : [And97]
% Names    : THM100 [And97]

% Status   : Unsatisfiable
% Rating   : 0.05 v5.2.0, 0.00 v2.5.0
% Syntax   : Number of clauses     :   16 (   0 non-Horn;   2 unit;  16 RR)
%            Number of atoms       :   30 (   0 equality)
%            Maximal clause size   :    2 (   2 average)
%            Number of predicates  :    1 (   0 propositional; 5-5 arity)
%            Number of functors    :    7 (   3 constant; 0-1 arity)
%            Number of variables   :   19 (   1 singleton)
%            Maximal term depth    :    2 (   1 average)
% SPC      : CNF_UNS_RFO_NEQ_HRN

% Comments :
%--------------------------------------------------------------------------
cnf(thm100_1,negated_conjecture,
    ( p(south,south,south,south,start) )).

cnf(thm100_2,negated_conjecture,
    ( p(north,north,south,north,go_alone(A))
    | ~ p(south,north,south,north,A) )).

cnf(thm100_3,negated_conjecture,
    ( p(south,north,south,north,go_alone(A))
    | ~ p(north,north,south,north,A) )).

cnf(thm100_4,negated_conjecture,
    ( p(north,south,north,south,go_alone(A))
    | ~ p(south,south,north,south,A) )).

cnf(thm100_5,negated_conjecture,
    ( p(south,south,north,south,go_alone(A))
    | ~ p(north,south,north,south,A) )).

cnf(thm100_6,negated_conjecture,
    ( p(north,north,south,north,take_wolf(A))
    | ~ p(south,south,south,north,A) )).

cnf(thm100_7,negated_conjecture,
    ( p(south,south,south,north,take_wolf(A))
    | ~ p(north,north,south,north,A) )).

cnf(thm100_8,negated_conjecture,
    ( p(north,north,north,south,take_wolf(A))
    | ~ p(south,south,north,south,A) )).

cnf(thm100_9,negated_conjecture,
    ( p(south,south,north,south,take_wolf(A))
    | ~ p(north,north,north,south,A) )).

cnf(thm100_10,negated_conjecture,
    ( p(north,A,north,B,take_goat(C))
    | ~ p(south,A,south,B,C) )).

cnf(thm100_11,negated_conjecture,
    ( p(south,A,south,B,take_goat(C))
    | ~ p(north,A,north,B,C) )).

cnf(thm100_12,negated_conjecture,
    ( p(north,north,south,north,take_cabbage(A))
    | ~ p(south,north,south,south,A) )).

cnf(thm100_13,negated_conjecture,
    ( p(south,north,south,south,take_cabbage(A))
    | ~ p(north,north,south,north,A) )).

cnf(thm100_14,negated_conjecture,
    ( p(north,south,north,north,take_cabbage(A))
    | ~ p(south,south,north,south,A) )).

cnf(thm100_15,negated_conjecture,
    ( p(south,south,north,south,take_cabbage(A))
    | ~ p(north,south,north,north,A) )).

cnf(thm100_16,negated_conjecture,
    ( ~ p(north,north,north,north,A) )).

%--------------------------------------------------------------------------
