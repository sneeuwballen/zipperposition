%--------------------------------------------------------------------------
% File     : PUZ035-2 : TPTP v5.2.0. Released v2.0.0.
% Domain   : Puzzles
% Problem  : Knights and Knaves #36
% Version  : [Sto95] axioms : Augmented.
% English  : On an island, there live exactly two types of people: knights
%            and knaves. Knights always tell the truth and knaves always
%            lie. I landed on the island, met two inhabitants, asked one of
%            them: "Is one of you a knight?" and he answered me. What can
%            be said about the types of the asked and the other person
%            depending on the answer I get?

% Refs     : [Smu78] Smullyan (1978), What is the Name of This Book? The Ri
%          : [Sto95] Stolzenburg (1995), Email to Geoff Sutcliffe.
%          : [BFS97] Baumgartner et al. (1997), Computing Answers with Mode
% Source   : [Sto95]
% Names    :

% Status   : Unsatisfiable
% Rating   : 0.00 v2.1.0
% Syntax   : Number of clauses     :   16 (   6 non-Horn;   2 unit;  16 RR)
%            Number of atoms       :   42 (   0 equality)
%            Maximal clause size   :    4 (   3 average)
%            Number of predicates  :    5 (   0 propositional; 1-2 arity)
%            Number of functors    :    6 (   6 constant; 0-0 arity)
%            Number of variables   :   12 (   6 singleton)
%            Maximal term depth    :    1 (   1 average)
% SPC      : CNF_UNS_EPR

% Comments : Contains one redundant clause. The query allows for
%            disjunctive answer A/X/Y = no/knave/knight;yes/knave/knave;
%            yes/knight/knave;yes/knight/knight
%--------------------------------------------------------------------------
%----Everyone's either a knight or a knave
cnf(everyone_a_knight_or_knave,axiom,
    ( ~ person(X)
    | isa(X,knight)
    | isa(X,knave) )).

cnf(not_both_a_knight_and_knave,axiom,
    ( ~ isa(X,knight)
    | ~ isa(X,knave) )).

cnf(knights_make_true_statements,axiom,
    ( ~ isa(X,knight)
    | tell_the_truth(X) )).

cnf(knaves_make_false_statements,axiom,
    ( ~ isa(X,knave)
    | lies(X) )).

cnf(statements_are_true_or_false,axiom,
    ( ~ tell_the_truth(X)
    | ~ lies(X) )).

%----This is redundant
cnf(statements_are_true_or_false2,axiom,
    ( ~ person(X)
    | tell_the_truth(X)
    | lies(X) )).

%----Possible answer scenarios
cnf(true_one_is_a_knight,axiom,
    ( isa(asked,knight)
    | isa(other,knight)
    | ~ response(yes)
    | ~ tell_the_truth(asked) )).

cnf(lie_one_is_a_knight,axiom,
    ( isa(asked,knight)
    | isa(other,knight)
    | ~ response(no)
    | ~ lies(asked) )).

cnf(knight_answers1,axiom,
    ( ~ response(yes)
    | ~ lies(asked)
    | ~ isa(Anyone,knight) )).

cnf(knight_answers2,axiom,
    ( ~ response(no)
    | ~ tell_the_truth(asked)
    | ~ isa(Anyone,knight) )).

cnf(knight_answers3,axiom,
    ( response(yes)
    | ~ tell_the_truth(asked)
    | ~ isa(Anyone,knight) )).

cnf(truthful_answer,axiom,
    ( response(no)
    | isa(asked,knight)
    | isa(other,knight)
    | ~ tell_the_truth(asked) )).

cnf(two_answers,axiom,
    ( response(yes)
    | response(no) )).

%----Two people
cnf(asked_person,axiom,
    ( person(asked) )).

cnf(other_person,axiom,
    ( person(other) )).

cnf(prove_answer,negated_conjecture,
    ( ~ response(A)
    | ~ isa(asked,X)
    | ~ isa(other,Y) )).

%--------------------------------------------------------------------------

