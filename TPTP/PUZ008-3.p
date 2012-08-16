%--------------------------------------------------------------------------
% File     : PUZ008-3 : TPTP v5.2.0. Released v1.2.0.
% Domain   : Puzzles
% Problem  : Missionaries and Cannibals
% Version  : Especial.
%            Theorem formulation : Declarative, without subsumers.
% English  : There are 3 missionaries, 3 cannibals, and a boat on the west
%            bank of a river. All wish to cross, but the boat holds
%            at most 2 people. If the cannibals ever outnumber the
%            missionaries on either bank of the river the outnumbered
%            missionaries will be eaten. Can they all safely cross the
%            river?  If so, how? (The boat cannot cross empty.)

% Refs     : [Rap95] Raptis (1995), Email to G. Sutcliffe
% Source   : [Rap95]
% Names    :

% Status   : Unsatisfiable
% Rating   : 0.15 v5.2.0, 0.08 v5.1.0, 0.06 v5.0.0, 0.07 v4.0.1, 0.00 v3.7.0, 0.14 v3.4.0, 0.20 v3.3.0, 0.00 v2.0.0
% Syntax   : Number of clauses     :   16 (   0 non-Horn;   4 unit;  14 RR)
%            Number of atoms       :   48 (   0 equality)
%            Maximal clause size   :    4 (   3 average)
%            Number of predicates  :    3 (   0 propositional; 2-3 arity)
%            Number of functors    :    8 (   3 constant; 0-2 arity)
%            Number of variables   :   47 (   3 singleton)
%            Maximal term depth    :    6 (   2 average)
% SPC      : CNF_UNS_RFO_NEQ_HRN

% Comments : This is a repaired version of PUZ008-1.
%--------------------------------------------------------------------------
%----Definition of safe
cnf(no_missionaries_is_safe,axiom,
    ( safe(n0,X) )).

cnf(same_or_more_missionaries_is_safe,axiom,
    ( ~ greater_or_equal(X,Y)
    | safe(X,Y) )).

%----Definition of less than or equal
cnf(great_eq_base,axiom,
    ( greater_or_equal(X,n0) )).

cnf(great_eq_recursive,axiom,
    ( greater_or_equal(s(X),s(Y))
    | ~ greater_or_equal(X,Y) )).

%----Moving cannibals only
cnf(cannibal_west_to_east,axiom,
    ( ~ achievable(west(m(X),c(s(Y))),boatonwest,east(m(Z),c(W)))
    | ~ safe(X,Y)
    | ~ safe(Z,s(W))
    | achievable(west(m(X),c(Y)),boatoneast,east(m(Z),c(s(W)))) )).

cnf(cannibal_east_to_west,axiom,
    ( ~ achievable(west(m(X),c(Y)),boatoneast,east(m(Z),c(s(W))))
    | ~ safe(X,s(Y))
    | ~ safe(Z,W)
    | achievable(west(m(X),c(s(Y))),boatonwest,east(m(Z),c(W))) )).

cnf(two_cannibals_west_to_east,axiom,
    ( ~ achievable(west(m(X),c(s(s(Y)))),boatonwest,east(m(Z),c(W)))
    | ~ safe(X,Y)
    | ~ safe(Z,s(s(W)))
    | achievable(west(m(X),c(Y)),boatoneast,east(m(Z),c(s(s(W))))) )).

cnf(two_cannibals_east_to_west,axiom,
    ( ~ achievable(west(m(X),c(Y)),boatoneast,east(m(Z),c(s(s(W)))))
    | ~ safe(X,s(s(Y)))
    | ~ safe(Z,W)
    | achievable(west(m(X),c(s(s(Y)))),boatonwest,east(m(Z),c(W))) )).

%----Moving missionaries only
cnf(missionary_west_to_east,axiom,
    ( ~ achievable(west(m(s(X)),c(Y)),boatonwest,east(m(Z),c(W)))
    | ~ safe(X,Y)
    | ~ safe(s(Z),W)
    | achievable(west(m(X),c(Y)),boatoneast,east(m(s(Z)),c(W))) )).

cnf(missionary_east_to_west,axiom,
    ( ~ achievable(west(m(X),c(Y)),boatoneast,east(m(s(Z)),c(W)))
    | ~ safe(s(X),Y)
    | ~ safe(Z,W)
    | achievable(west(m(s(X)),c(Y)),boatonwest,east(m(Z),c(W))) )).

cnf(two_missionaries_west_to_east,axiom,
    ( ~ achievable(west(m(s(s(X))),c(Y)),boatonwest,east(m(Z),c(W)))
    | ~ safe(X,Y)
    | ~ safe(s(s(Z)),W)
    | achievable(west(m(X),c(Y)),boatoneast,east(m(s(s(Z))),c(W))) )).

cnf(two_missionaries_east_to_west,axiom,
    ( ~ achievable(west(m(X),c(Y)),boatoneast,east(m(s(s(Z))),c(W)))
    | ~ safe(s(s(X)),Y)
    | ~ safe(Z,W)
    | achievable(west(m(s(s(X))),c(Y)),boatonwest,east(m(Z),c(W))) )).

%----Moving a missionary and a cannibal
cnf(missionary_and_cannibal_west_to_east,axiom,
    ( ~ achievable(west(m(s(X)),c(s(Y))),boatonwest,east(m(Z),c(W)))
    | ~ safe(X,Y)
    | ~ safe(s(Z),s(W))
    | achievable(west(m(X),c(Y)),boatoneast,east(m(s(Z)),c(s(W)))) )).

cnf(missionary_and_cannibal_east_to_west,axiom,
    ( ~ achievable(west(m(X),c(Y)),boatoneast,east(m(s(Z)),c(s(W))))
    | ~ safe(s(X),s(Y))
    | ~ safe(Z,W)
    | achievable(west(m(s(X)),c(s(Y))),boatonwest,east(m(Z),c(W))) )).

%----The passive clauses are removed
%----input_clause(extra_cannibal_meal_on_west_bank,axiom,
%----    [++achievable(west(m(s(X)),c(s(s(X)))),Y,east(Z,W))]).
%----
%----input_clause(two_extra_cannibals_meal_on_west_bank,axiom,
%----    [++achievable(west(m(s(X)),c(s(s(s(X))))),Y,east(Z,W))]).
%----
%----input_clause(extra_cannibal_meal_on_east_bank,axiom,
%----    [++achievable(west(X,Y),Z,east(m(s(W)),c(s(s(W)))))]).
%----
%----input_clause(two_extra_cannibals_meal_on_east_bank,axiom,
%----    [++achievable(west(X,Y),Z,east(m(s(W)),c(s(s(s(W))))))]).

%----Initial state
cnf(start_on_west_bank,hypothesis,
    ( achievable(west(m(s(s(s(n0)))),c(s(s(s(n0))))),boatonwest,east(m(n0),c(n0))) )).

%----Final state
cnf(prove_can_get_to_east_bank,negated_conjecture,
    ( ~ achievable(west(m(n0),c(n0)),X,east(m(s(s(s(n0)))),c(s(s(s(n0)))))) )).

%--------------------------------------------------------------------------
