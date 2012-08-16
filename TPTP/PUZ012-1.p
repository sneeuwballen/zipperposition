%--------------------------------------------------------------------------
% File     : PUZ012-1 : TPTP v5.2.0. Bugfixed v1.2.1.
% Domain   : Puzzles
% Problem  : The Mislabeled Boxes
% Version  : Especial.
% English  : There are three boxes a, b, and c on a table. Each box contains
%            apples or bananas or oranges. No two boxes contain the same
%            thing. Each box has a label that says it contains apples or says
%            it contains bananas or says it contains oranges. No box contains
%            what it says on its label. The label on box a says "apples".
%            The label on box b says "oranges". The label on box c says
%            "bananas". You pick up box b and it contains apples. What do
%            the other two boxes contain?

% Refs     : [WO+92] Wos et al. (1992), Automated Reasoning: Introduction a
%          : [Wos88] Wos (1988), Automated Reasoning - 33 Basic Research Pr
% Source   : [ANL]
% Names    : Boxes-of-fruit [WO+92]
%          : Boxes-of-fruit [Wos88]
%          : boxes.ver1.in [ANL]

% Status   : Unsatisfiable
% Rating   : 0.00 v2.0.0
% Syntax   : Number of clauses     :   18 (   2 non-Horn;  12 unit;  14 RR)
%            Number of atoms       :   28 (   0 equality)
%            Maximal clause size   :    3 (   2 average)
%            Number of predicates  :    4 (   0 propositional; 2-2 arity)
%            Number of functors    :    6 (   6 constant; 0-0 arity)
%            Number of variables   :   12 (   0 singleton)
%            Maximal term depth    :    1 (   1 average)
% SPC      : CNF_UNS_EPR

% Comments :
% Bugfixes : v1.2.1 - Theorem clause uncommented (commented out during some
%            local testing, and forgot to uncomment it again).
%--------------------------------------------------------------------------
cnf(reflexivity_for_fruits,axiom,
    ( equal_fruits(X,X) )).

cnf(reflexivity_for_boxes,axiom,
    ( equal_boxes(X,X) )).

cnf(label_is_wrong,axiom,
    ( ~ label(X,Y)
    | ~ contains(X,Y) )).

cnf(each_thing_is_in_a_box,axiom,
    ( contains(boxa,X)
    | contains(boxb,X)
    | contains(boxc,X) )).

cnf(each_box_contains_something,axiom,
    ( contains(X,apples)
    | contains(X,bananas)
    | contains(X,oranges) )).

cnf(contains_is_well_defined1,axiom,
    ( ~ contains(X,Y)
    | ~ contains(X,Z)
    | equal_fruits(Y,Z) )).

cnf(contains_is_well_defined2,axiom,
    ( ~ contains(X,Y)
    | ~ contains(Z,Y)
    | equal_boxes(X,Z) )).

cnf(boxa_not_boxb,axiom,
    ( ~ equal_boxes(boxa,boxb) )).

cnf(boxb_not_boxc,axiom,
    ( ~ equal_boxes(boxb,boxc) )).

cnf(boxa_not_boxc,axiom,
    ( ~ equal_boxes(boxa,boxc) )).

cnf(apples_not_bananas,axiom,
    ( ~ equal_fruits(apples,bananas) )).

cnf(bananas_not_oranges,axiom,
    ( ~ equal_fruits(bananas,oranges) )).

cnf(apples_not_oranges,axiom,
    ( ~ equal_fruits(apples,oranges) )).

cnf(boxa_labelled_apples,hypothesis,
    ( label(boxa,apples) )).

cnf(boxb_labelled_oranges,hypothesis,
    ( label(boxb,oranges) )).

cnf(boxc_labelled_bananas,hypothesis,
    ( label(boxc,bananas) )).

cnf(boxb_contains_apples,hypothesis,
    ( contains(boxb,apples) )).

cnf(prove_boxa_contains_bananas_and_boxc_oranges,negated_conjecture,
    ( ~ contains(boxa,bananas)
    | ~ contains(boxc,oranges) )).

%--------------------------------------------------------------------------
