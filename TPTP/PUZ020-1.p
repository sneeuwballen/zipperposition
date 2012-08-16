%--------------------------------------------------------------------------
% File     : PUZ020-1 : TPTP v5.2.0. Released v1.0.0.
% Domain   : Puzzles
% Problem  : A knights & knaves problem, if he's a knight, so is she
% Version  : Especial.
% English  :

% Refs     : [Rap95] Raptis (1995), Email to G. Sutcliffe
% Source   : [ANL]
% Names    : knightknave.in [ANL]

% Status   : Unsatisfiable
% Rating   : 0.06 v5.2.0, 0.00 v2.0.0
% Syntax   : Number of clauses     :   19 (   3 non-Horn;   5 unit;  19 RR)
%            Number of atoms       :   41 (   4 equality)
%            Maximal clause size   :    3 (   2 average)
%            Number of predicates  :    6 (   0 propositional; 1-2 arity)
%            Number of functors    :    3 (   2 constant; 0-1 arity)
%            Number of variables   :   16 (   2 singleton)
%            Maximal term depth    :    2 (   1 average)
% SPC      : CNF_UNS_RFO_SEQ_NHN

% Comments : Dimitris Raptis [Rap95] has pointed out that the clause
%            statements_are_true_or_not is a tautology. Does your ATP system
%            ignore it?
%--------------------------------------------------------------------------
%----This axiom is omitted in the orginal, because of paramodulation
%----Everyone's either a knight or a knave; statements are true or false.
cnf(everyone_a_knight_or_knave,axiom,
    ( ~ person(X)
    | knight(X)
    | knave(X) )).

cnf(not_both_a_knight_and_knave,axiom,
    ( ~ person(X)
    | ~ knight(X)
    | ~ knave(X) )).

cnf(statements_are_true_or_false,axiom,
    ( ~ says(X,Y)
    | a_truth(Y)
    | ~ a_truth(Y) )).

%----Rules about statements
cnf(people_do_not_equal_their_statements1,axiom,
    ( ~ says(X,Y)
    | X != Y )).

cnf(peoples_statements,axiom,
    ( ~ says(X,Y)
    | Y = statement_by(X) )).

cnf(people_do_not_equal_their_statement2,axiom,
    ( ~ person(X)
    | X != statement_by(Y) )).

cnf(knights_make_true_statements,axiom,
    ( ~ person(X)
    | ~ a_truth(statement_by(X))
    | knight(X) )).

cnf(knaves_make_false_statements,axiom,
    ( ~ person(X)
    | a_truth(statement_by(X))
    | knave(X) )).

%----What knights say is true, what knaves say is false
cnf(knights_say_the_truth,axiom,
    ( ~ knight(X)
    | ~ says(X,Y)
    | a_truth(Y) )).

cnf(knaves_do_not_say_the_truth,axiom,
    ( ~ knave(X)
    | ~ says(X,Y)
    | ~ a_truth(Y) )).

%----This is for the husband & wife problem; if he's a knight, then she
%----is
cnf(husband,hypothesis,
    ( person(husband) )).

cnf(wife,hypothesis,
    ( person(wife) )).

cnf(husband_not_wife,hypothesis,
    (  husband != wife )).

cnf(husband_makes_statements,hypothesis,
    ( says(husband,statement_by(husband)) )).

cnf(truthful_knight_husband_means_knight_wife,hypothesis,
    ( ~ a_truth(statement_by(husband))
    | ~ knight(husband)
    | knight(wife) )).

cnf(knight_husband_makes_true_statements,hypothesis,
    ( a_truth(statement_by(husband))
    | ~ knight(husband) )).

cnf(knight_wife_or_truthful_husband,hypothesis,
    ( a_truth(statement_by(husband))
    | knight(wife) )).

cnf(knight_wife_means_truthful_husband,hypothesis,
    ( ~ knight(wife)
    | a_truth(statement_by(husband)) )).

cnf(prove_knight_husband,negated_conjecture,
    ( ~ knight(husband) )).

%--------------------------------------------------------------------------
