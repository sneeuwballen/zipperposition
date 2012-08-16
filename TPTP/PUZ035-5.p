%--------------------------------------------------------------------------
% File     : PUZ035-5 : TPTP v5.2.0. Released v2.0.0.
% Domain   : Puzzles
% Problem  : Knights and Knaves #36
% Version  : [Sto95] axioms.
%            Theorem formulation : Definite answer "yes".
% English  : On an island, there live exactly two types of people: knights
%            and knaves. Knights always tell the truth and knaves always
%            lie. I landed on the island, met two inhabitants, asked one of
%            them: "Is one of you a knight?" and he answered me. What can
%            be said about the types of the asked and the other person
%            depending on the answer I get?

% Refs     : [Smu78] Smullyan (1978), What is the Name of This Book? The Ri
%          : [Sto95] Stolzenburg (1995), Email to Geoff Sutcliffe.
%          : [BFS95] Baumgartner et al. (1995), Model Elimination, Logic Pr
%          : [BFS97] Baumgartner et al. (1997), Computing Answers with Mode
% Source   : [Sto95]
% Names    :

% Status   : Unsatisfiable
% Rating   : 0.00 v2.1.0
% Syntax   : Number of clauses     :    9 (   2 non-Horn;   1 unit;   6 RR)
%            Number of atoms       :   20 (   0 equality)
%            Maximal clause size   :    3 (   2 average)
%            Number of predicates  :    2 (   0 propositional; 1-2 arity)
%            Number of functors    :    6 (   4 constant; 0-2 arity)
%            Number of variables   :   14 (   4 singleton)
%            Maximal term depth    :    3 (   2 average)
% SPC      : CNF_UNS_RFO_NEQ_NHN

% Comments : Query allows for disjunctive answer X/Y = knave/knave ;
%            knight/knave ; knight/knight
%--------------------------------------------------------------------------
%----Everyone's either a knight or a knave
cnf(everyone_a_knight_or_knave,axiom,
    ( truth(isa(P,knight))
    | truth(isa(P,knave)) )).

cnf(not_both_a_knight_and_knave,axiom,
    ( ~ truth(isa(P,knight))
    | ~ truth(isa(P,knave)) )).

cnf(knights_make_true_statements1,axiom,
    ( truth(S)
    | ~ truth(isa(P,knight))
    | ~ says(P,S) )).

cnf(knights_make_true_statements2,axiom,
    ( truth(isa(P,knight))
    | ~ truth(S)
    | ~ says(P,S) )).

%----Definitions for or
cnf(or1,axiom,
    ( truth(A)
    | truth(B)
    | ~ truth(or(A,B)) )).

cnf(or2,axiom,
    ( truth(or(A,B))
    | ~ truth(A) )).

cnf(or3,axiom,
    ( truth(or(A,B))
    | ~ truth(B) )).

cnf(says_yes,axiom,
    ( says(asked,or(isa(asked,knight),isa(other,knight))) )).

cnf(query,negated_conjecture,
    ( ~ truth(isa(asked,X))
    | ~ truth(isa(other,Y)) )).

%--------------------------------------------------------------------------
