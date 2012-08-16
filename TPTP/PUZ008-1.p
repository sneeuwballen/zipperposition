%--------------------------------------------------------------------------
% File     : PUZ008-1 : TPTP v5.2.0. Released v1.0.0.
% Domain   : Puzzles
% Problem  : Missionaries and Cannibals
% Version  : Especial.
%            Theorem formulation : Declarative.
% English  : There are 3 missionaries, 3 cannibals, and a boat on the west
%            bank of a river. All wish to cross, but the boat holds
%            at most 2 people. If the cannibals ever outnumber the
%            missionaries on either bank of the river the outnumbered
%            missionaries will be eaten. Can they all safely cross the
%            river?  If so, how? (The boat cannot cross empty.)

% Refs     : [WO+92] Wos et al. (1992), Automated Reasoning: Introduction a
%          : [Rap95] Raptis (1995), Email to G. Sutcliffe
% Source   : [ANL]
% Names    : mission.ver1.in [ANL]

% Status   : Unsatisfiable
% Rating   : 0.05 v5.2.0, 0.00 v3.7.0, 0.14 v3.4.0, 0.20 v3.3.0, 0.00 v2.0.0
% Syntax   : Number of clauses     :   16 (   0 non-Horn;   6 unit;  12 RR)
%            Number of atoms       :   26 (   0 equality)
%            Maximal clause size   :    2 (   2 average)
%            Number of predicates  :    1 (   0 propositional; 3-3 arity)
%            Number of functors    :    8 (   3 constant; 0-2 arity)
%            Number of variables   :   57 (  13 singleton)
%            Maximal term depth    :    6 (   3 average)
% SPC      : CNF_UNS_RFO_NEQ_HRN

% Comments : Dimitris Raptis has pointed out [Rap95] that this formulation
%            allows a trivial solution to be found. This is due to 4 clauses
%            that [WO+92] add in for forward subsumption purposes only (I
%            don't know of any ATP system that has a "for subsumption only"
%            subset of the input clauses - might be worth researching). If
%            those clauses are omitted, an illegal solution can be found.
%--------------------------------------------------------------------------
%----Moving cannibals only
cnf(cannibal_west_to_east,axiom,
    ( ~ achievable(west(m(X),c(s(Y))),boatonwest,east(m(Z),c(W)))
    | achievable(west(m(X),c(Y)),boatoneast,east(m(Z),c(s(W)))) )).

cnf(cannibal_east_to_west,axiom,
    ( ~ achievable(west(m(X),c(Y)),boatoneast,east(m(Z),c(s(W))))
    | achievable(west(m(X),c(s(Y))),boatonwest,east(m(Z),c(W))) )).

cnf(two_cannibals_west_to_east,axiom,
    ( ~ achievable(west(m(X),c(s(s(Y)))),boatonwest,east(m(Z),c(W)))
    | achievable(west(m(X),c(Y)),boatoneast,east(m(Z),c(s(s(W))))) )).

cnf(two_cannibals_east_to_west,axiom,
    ( ~ achievable(west(m(X),c(Y)),boatoneast,east(m(Z),c(s(s(W)))))
    | achievable(west(m(X),c(s(s(Y)))),boatonwest,east(m(Z),c(W))) )).

%----Moving missionaries only
cnf(missionary_west_to_east,axiom,
    ( ~ achievable(west(m(s(X)),c(Y)),boatonwest,east(m(Z),c(W)))
    | achievable(west(m(X),c(Y)),boatoneast,east(m(s(Z)),c(W))) )).

cnf(missionary_east_to_west,axiom,
    ( ~ achievable(west(m(X),c(Y)),boatoneast,east(m(s(Z)),c(W)))
    | achievable(west(m(s(X)),c(Y)),boatonwest,east(m(Z),c(W))) )).

cnf(two_missionaries_west_to_east,axiom,
    ( ~ achievable(west(m(s(s(X))),c(Y)),boatonwest,east(m(Z),c(W)))
    | achievable(west(m(X),c(Y)),boatoneast,east(m(s(s(Z))),c(W))) )).

cnf(two_missionaries_east_to_west,axiom,
    ( ~ achievable(west(m(X),c(Y)),boatoneast,east(m(s(s(Z))),c(W)))
    | achievable(west(m(s(s(X))),c(Y)),boatonwest,east(m(Z),c(W))) )).

%----Moving a missionary and a cannibal
cnf(missionary_and_cannibal_west_to_east,axiom,
    ( ~ achievable(west(m(s(X)),c(s(Y))),boatonwest,east(m(Z),c(W)))
    | achievable(west(m(X),c(Y)),boatoneast,east(m(s(Z)),c(s(W)))) )).

cnf(missionary_and_cannibal_east_to_west,axiom,
    ( ~ achievable(west(m(X),c(Y)),boatoneast,east(m(s(Z)),c(s(W))))
    | achievable(west(m(s(X)),c(s(Y))),boatonwest,east(m(Z),c(W))) )).

%----The next four clauses should only be used for forward subsumption.
%----In OTTER they would be put in the passive list. For other ATP
%----systems, these clauses allow a trivial solution to be found. But
%----if they are omitted, an illegal solution can be found.
cnf(extra_cannibal_meal_on_west_bank,axiom,
    ( achievable(west(m(s(X)),c(s(s(X)))),Y,east(Z,W)) )).

cnf(two_extra_cannibals_meal_on_west_bank,axiom,
    ( achievable(west(m(s(X)),c(s(s(s(X))))),Y,east(Z,W)) )).

cnf(extra_cannibal_meal_on_east_bank,axiom,
    ( achievable(west(X,Y),Z,east(m(s(W)),c(s(s(W))))) )).

cnf(two_extra_cannibals_meal_on_east_bank,axiom,
    ( achievable(west(X,Y),Z,east(m(s(W)),c(s(s(s(W)))))) )).

cnf(start_on_west_bank,hypothesis,
    ( achievable(west(m(s(s(s(n0)))),c(s(s(s(n0))))),boatonwest,east(m(n0),c(n0))) )).

cnf(prove_can_get_to_east_bank,negated_conjecture,
    ( ~ achievable(west(m(n0),c(n0)),X,east(m(s(s(s(n0)))),c(s(s(s(n0)))))) )).

%--------------------------------------------------------------------------
