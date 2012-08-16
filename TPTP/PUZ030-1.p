%--------------------------------------------------------------------------
% File     : PUZ030-1 : TPTP v5.2.0. Released v1.0.0.
% Domain   : Puzzles
% Problem  : Salt and Mustard Problem
% Version  : Especial.
% English  :

% Refs     : [LO85]  Lusk & Overbeek (1985), Non-Horn Problems
%          : [Car86] Carroll (1986), Lewis Carroll's Symbolic Logic
%          : [MB88]  Manthey & Bry (1988), SATCHMO: A Theorem Prover Implem
% Source   : [LO85]
% Names    : Salt and Mustard Problem [LO85]
%          : Salt-and-Mustard Problem [MB88]

% Status   : Unsatisfiable
% Rating   : 0.00 v2.2.0, 0.25 v2.1.0, 0.00 v2.0.0
% Syntax   : Number of clauses     :   43 (  13 non-Horn;   0 unit;  41 RR)
%            Number of atoms       :  106 (   0 equality)
%            Maximal clause size   :    7 (   2 average)
%            Number of predicates  :    5 (   0 propositional; 1-1 arity)
%            Number of functors    :    5 (   5 constant; 0-0 arity)
%            Number of variables   :   12 (   0 singleton)
%            Maximal term depth    :    1 (   1 average)
% SPC      : CNF_UNS_EPR

% Comments :
%--------------------------------------------------------------------------
cnf(both1,axiom,
    ( ~ both(X)
    | salt(X) )).

cnf(both2,axiom,
    ( ~ both(X)
    | mustard(X) )).

cnf(both3,axiom,
    ( ~ salt(X)
    | ~ mustard(X)
    | both(X) )).

cnf(oneof1,axiom,
    ( ~ oneof(X)
    | salt(X)
    | mustard(X) )).

cnf(oneof2,axiom,
    ( ~ oneof(X)
    | ~ both(X) )).

cnf(oneof3,axiom,
    ( ~ oneof(X)
    | ~ neither(X) )).

cnf(one_condition_holds1,axiom,
    ( both(X)
    | neither(X)
    | oneof(X) )).

cnf(oneof4,axiom,
    ( ~ oneof(X)
    | ~ salt(X)
    | ~ mustard(X) )).

cnf(neither1,axiom,
    ( ~ both(X)
    | ~ neither(X) )).

cnf(neither2,axiom,
    ( ~ neither(X)
    | ~ salt(X) )).

cnf(neither3,axiom,
    ( ~ neither(X)
    | ~ mustard(X) )).

cnf(neither4,axiom,
    ( salt(X)
    | mustard(X)
    | neither(X) )).

cnf(rule1_1,hypothesis,
    ( ~ salt(barry)
    | oneof(cole)
    | oneof(lang) )).

cnf(rule1_2,hypothesis,
    ( ~ oneof(cole)
    | salt(barry) )).

cnf(rule1_3,hypothesis,
    ( ~ oneof(lang)
    | salt(barry) )).

cnf(rule2_1,hypothesis,
    ( ~ mustard(barry)
    | neither(dix)
    | both(mill) )).

cnf(rule2_2,hypothesis,
    ( ~ neither(dix)
    | mustard(barry) )).

cnf(rule2_3,hypothesis,
    ( ~ both(mill)
    | mustard(barry) )).

cnf(rule3_1,hypothesis,
    ( ~ salt(cole)
    | oneof(barry)
    | neither(mill) )).

cnf(rule3_2,hypothesis,
    ( ~ oneof(barry)
    | salt(cole) )).

cnf(rule3_3,hypothesis,
    ( ~ neither(mill)
    | salt(cole) )).

cnf(rule4_1,hypothesis,
    ( ~ mustard(cole)
    | both(dix)
    | both(lang) )).

cnf(rule4_2,hypothesis,
    ( ~ both(dix)
    | mustard(cole) )).

cnf(rule4_3,hypothesis,
    ( ~ both(lang)
    | mustard(cole) )).

cnf(rule5_1,hypothesis,
    ( ~ salt(dix)
    | neither(barry)
    | both(cole) )).

cnf(rule5_2,hypothesis,
    ( ~ neither(barry)
    | salt(dix) )).

cnf(rule5_3,hypothesis,
    ( ~ both(cole)
    | salt(dix) )).

cnf(rule6_1,hypothesis,
    ( ~ mustard(dix)
    | neither(lang)
    | neither(mill) )).

cnf(rule6_2,hypothesis,
    ( ~ neither(lang)
    | mustard(dix) )).

cnf(rule6_3,hypothesis,
    ( ~ neither(mill)
    | mustard(dix) )).

cnf(rule7_1,hypothesis,
    ( ~ salt(lang)
    | oneof(barry)
    | oneof(dix) )).

cnf(rule7_2,hypothesis,
    ( ~ oneof(barry)
    | salt(lang) )).

cnf(rule7_3,hypothesis,
    ( ~ oneof(dix)
    | salt(lang) )).

cnf(rule8_1,hypothesis,
    ( ~ mustard(lang)
    | neither(cole)
    | neither(mill) )).

cnf(rule8_2,hypothesis,
    ( ~ neither(cole)
    | mustard(lang) )).

cnf(rule8_3,hypothesis,
    ( ~ neither(mill)
    | mustard(lang) )).

cnf(rule9_1,hypothesis,
    ( ~ salt(mill)
    | both(barry)
    | both(lang) )).

cnf(rule9_2,hypothesis,
    ( ~ both(barry)
    | salt(mill) )).

cnf(rule9_3,hypothesis,
    ( ~ both(lang)
    | mustard(mill) )).

cnf(rule10_1,hypothesis,
    ( ~ mustard(mill)
    | oneof(cole)
    | oneof(dix) )).

cnf(rule10_2,hypothesis,
    ( ~ oneof(cole)
    | mustard(mill) )).

cnf(rule10_3,hypothesis,
    ( ~ oneof(dix)
    | mustard(mill) )).

cnf(prove_who_takes_what,negated_conjecture,
    ( ~ neither(cole)
    | ~ neither(dix)
    | ~ both(barry)
    | ~ oneof(lang)
    | ~ salt(mill)
    | ~ mustard(lang)
    | ~ oneof(mill) )).

%--------------------------------------------------------------------------
