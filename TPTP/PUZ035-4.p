%--------------------------------------------------------------------------
% File     : PUZ035-4 : TPTP v5.2.0. Released v2.0.0.
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
% Rating   : 0.10 v5.2.0, 0.00 v5.1.0, 0.09 v5.0.0, 0.14 v4.1.0, 0.00 v4.0.1, 0.20 v4.0.0, 0.14 v3.4.0, 0.25 v3.3.0, 0.33 v3.1.0, 0.17 v2.7.0, 0.12 v2.6.0, 0.33 v2.5.0, 0.20 v2.4.0, 0.00 v2.1.0
% Syntax   : Number of clauses     :   14 (   5 non-Horn;   2 unit;  14 RR)
%            Number of atoms       :   33 (   0 equality)
%            Maximal clause size   :    3 (   2 average)
%            Number of predicates  :    3 (   0 propositional; 1-2 arity)
%            Number of functors    :    7 (   4 constant; 0-2 arity)
%            Number of variables   :   20 (   7 singleton)
%            Maximal term depth    :    4 (   2 average)
% SPC      : CNF_UNS_RFO_NEQ_NHN

% Comments : The query allows for disjunctive answer P1/P2/S =
%            knave/knave/* ; knave/knight/not(*) ; knight/knave/* ;
%            knight/knight/*, where * stands for
%            or(isa(asked,knight),isa(other,knight))
%--------------------------------------------------------------------------
%----Everyone's either a knight or a knave
cnf(everyone_a_knight_or_knave,axiom,
    ( ~ person(P)
    | truth(isa(P,knight))
    | truth(isa(P,knave)) )).

cnf(not_both_a_knight_and_knave,axiom,
    ( ~ truth(isa(P,knight))
    | ~ truth(isa(P,knave)) )).

cnf(knights_make_true_statements1,axiom,
    ( truth(S)
    | ~ says(P,S)
    | ~ truth(isa(P,knight)) )).

cnf(knights_make_true_statements2,axiom,
    ( truth(isa(P,knight))
    | ~ says(P,S)
    | ~ truth(S) )).

%----Possible answer scenarios
cnf(one_is_knight_answer,axiom,
    ( says(asked,or(isa(asked,knight),isa(other,knight)))
    | says(asked,not(or(isa(asked,knight),isa(other,knight)))) )).

%----Axioms of truth and statements
cnf(statements_are_true_or_false,axiom,
    ( truth(S)
    | truth(not(S))
    | ~ says(Anyone,S) )).

cnf(statements_are_true_or_false2,axiom,
    ( truth(S)
    | truth(not(S))
    | ~ says(Anyone,not(S)) )).

%----Replaces metarule
cnf(statements_are_true_or_false3,axiom,
    ( ~ truth(S)
    | ~ truth(not(S)) )).

%----Axioms for or and not
cnf(or_def,axiom,
    ( truth(S1)
    | truth(S2)
    | ~ truth(or(S1,S2)) )).

cnf(not1,axiom,
    ( ~ truth(S1)
    | ~ truth(not(or(S1,Anything))) )).

cnf(not2,axiom,
    ( ~ truth(S2)
    | ~ truth(not(or(Anything,S2))) )).

%----Two people
cnf(asked_person,axiom,
    ( person(asked) )).

cnf(other_person,axiom,
    ( person(other) )).

cnf(prove_answer,negated_conjecture,
    ( ~ truth(isa(asked,P1))
    | ~ truth(isa(other,P2))
    | ~ says(asked,S) )).

%--------------------------------------------------------------------------
