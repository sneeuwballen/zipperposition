%--------------------------------------------------------------------------
% File     : PUZ021-1 : TPTP v5.2.0. Released v1.0.0.
% Domain   : Puzzles
% Problem  : How to Win a Bride
% Version  : Especial.
% English  : Suppose you are an inhabitant of the island of 'knights' and
%            'knaves'. The knights always tell the truth and the knaves
%            always lie. You fall in love with a girl there and wish
%            to marry her. However, this girl has strange tastes; for some
%            odd reason she does not wish to marry a knight; she wants
%            to marry only a knave. But she wants a rich knave, not a poor
%            one. (We assume for convenience that everyone is classified
%            as either rich or poor.) Suppose, in fact, that you are
%            a rich knave. You are allowed to make only one statement, can
%            you convince her that you are a rich knave?

% Refs     : [Smu78] Smullyan (1978), What is the Name of This Book? The Ri
%          : [Ohl85] Ohlbach (1985), Predicate Logic Hacker Tricks
% Source   : [Ohl85]
% Names    : Problem 95 [Smu78]
%          : How to Win a Bride [Ohl85]

% Status   : Unsatisfiable
% Rating   : 0.00 v5.1.0, 0.09 v5.0.0, 0.07 v4.1.0, 0.00 v2.5.0, 0.20 v2.4.0, 0.00 v2.2.1, 0.25 v2.1.0, 0.62 v2.0.0
% Syntax   : Number of clauses     :   13 (   4 non-Horn;   0 unit;   9 RR)
%            Number of atoms       :   31 (   0 equality)
%            Maximal clause size   :    3 (   2 average)
%            Number of predicates  :    2 (   0 propositional; 2-2 arity)
%            Number of functors    :    6 (   1 constant; 0-2 arity)
%            Number of variables   :   31 (   2 singleton)
%            Maximal term depth    :    3 (   1 average)
% SPC      : CNF_UNS_RFO_NEQ_NHN

% Comments :
%--------------------------------------------------------------------------
cnf(not_knight_and_knave,axiom,
    ( ~ a_truth(knight(X),Y)
    | ~ a_truth(knave(X),Y) )).

cnf(knight_or_knave,axiom,
    ( a_truth(knight(X),Y)
    | a_truth(knave(X),Y) )).

cnf(not_rich_and_poor,axiom,
    ( ~ a_truth(rich(X),Y)
    | ~ a_truth(poor(X),Y) )).

cnf(rich_or_poor,axiom,
    ( a_truth(rich(X),Y)
    | a_truth(poor(X),Y) )).

cnf(knights_tell_truth1,axiom,
    ( ~ a_truth(knight(X),Z)
    | ~ says(X,Y)
    | a_truth(Y,Z) )).

cnf(knights_tell_truth2,axiom,
    ( ~ a_truth(knight(X),Z)
    | says(X,Y)
    | ~ a_truth(Y,Z) )).

cnf(knaves_lie1,axiom,
    ( ~ a_truth(knave(X),Z)
    | ~ says(X,Y)
    | ~ a_truth(Y,Z) )).

cnf(knaves_lie2,axiom,
    ( ~ a_truth(knave(X),Z)
    | says(X,Y)
    | a_truth(Y,Z) )).

cnf(conjunction1,axiom,
    ( ~ a_truth(and(X,Y),Z)
    | a_truth(X,Z) )).

cnf(conjunction2,axiom,
    ( ~ a_truth(and(X,Y),Z)
    | a_truth(Y,Z) )).

cnf(conjunction3,axiom,
    ( a_truth(and(X,Y),Z)
    | ~ a_truth(X,Z)
    | ~ a_truth(Y,Z) )).

cnf(prove_statement_exists1,negated_conjecture,
    ( ~ says(me,X)
    | ~ a_truth(and(knave(me),rich(me)),X) )).

cnf(prove_statement_exists2,negated_conjecture,
    ( says(me,X)
    | a_truth(and(knave(me),rich(me)),X) )).

%--------------------------------------------------------------------------
