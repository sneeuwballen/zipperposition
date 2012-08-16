%--------------------------------------------------------------------------
% File     : PUZ035-7 : TPTP v5.2.0. Released v2.0.0.
% Domain   : Puzzles
% Problem  : Knights and Knaves #36
% Version  : [Sto95] axioms.
%            Theorem formulation : All possibilities at once.
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
% Rating   : 0.10 v5.2.0, 0.00 v5.0.0, 0.07 v4.1.0, 0.00 v4.0.1, 0.20 v4.0.0, 0.14 v3.4.0, 0.25 v3.3.0, 0.33 v2.7.0, 0.00 v2.6.0, 0.33 v2.5.0, 0.20 v2.4.0, 0.00 v2.1.0
% Syntax   : Number of clauses     :   14 (   5 non-Horn;   0 unit;   8 RR)
%            Number of atoms       :   34 (   0 equality)
%            Maximal clause size   :    3 (   2 average)
%            Number of predicates  :    2 (   0 propositional; 1-3 arity)
%            Number of functors    :    9 (   6 constant; 0-2 arity)
%            Number of variables   :   25 (   5 singleton)
%            Maximal term depth    :    3 (   1 average)
% SPC      : CNF_UNS_RFO_NEQ_NHN

% Comments : Comprises the whole problem in one description. The query
%            allows for disjunctive answer R/X/Y = no/knave/knight;
%            yes/knave/knave;yes/knight/knave;yes/knight/knight
%--------------------------------------------------------------------------
%----On the island, there live exactly two types of people: knights and
%----knaves.
cnf(everyone_a_knight_or_knave,axiom,
    ( truth(isa(P,knight))
    | truth(isa(P,knave)) )).

cnf(not_both_a_knight_and_knave,axiom,
    ( ~ truth(isa(P,knight))
    | ~ truth(isa(P,knave)) )).

%----Knights always tell the truth and knaves always lie.
%----Formally: truth(Q) <=> truth(isa(P,knight)) <=> reply(P,Q,yes)
cnf(knights_make_true_statements1,axiom,
    ( truth(Q)
    | ~ truth(isa(P,knight))
    | ~ reply(P,Q,yes) )).

cnf(knights_make_true_statements2,axiom,
    ( ~ truth(Q)
    | truth(isa(P,knight))
    | ~ reply(P,Q,yes) )).

cnf(knights_make_true_statements3,axiom,
    ( ~ truth(Q)
    | ~ truth(isa(P,knight))
    | reply(P,Q,yes) )).

cnf(knights_make_true_statements4,axiom,
    ( truth(Q)
    | truth(isa(P,knight))
    | reply(P,Q,yes) )).

%----Every inhabitant answers a question with a straight yes or no.
cnf(yes_or_no,axiom,
    ( reply(P,Q,yes)
    | reply(P,Q,no) )).

cnf(not_yes_and_no,axiom,
    ( ~ reply(P,Q,yes)
    | ~ reply(P,Q,no) )).

%----Definitions for not and truth
cnf(true1,axiom,
    ( truth(C)
    | truth(not(C)) )).

cnf(true2,axiom,
    ( ~ truth(C)
    | ~ truth(not(C)) )).

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

%----This is the query. It is easy to state than other knights-and-knaves
%----problems.
cnf(prove_answer,negated_conjecture,
    ( ~ reply(asked,or(isa(asked,knight),isa(other,knight)),R)
    | ~ truth(isa(asked,X))
    | ~ truth(isa(other,Y)) )).

%--------------------------------------------------------------------------
