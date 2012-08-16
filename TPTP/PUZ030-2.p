%--------------------------------------------------------------------------
% File     : PUZ030-2 : TPTP v5.2.0. Released v1.0.0.
% Domain   : Puzzles
% Problem  : Salt and Mustard Problem
% Version  : Especial.
%            Theorem formulation : Propositional.
% English  :

% Refs     : [Car86] Carroll (1986), Lewis Carroll's Symbolic Logic
% Source   : [TPTP]
% Names    : salt.in [OTTER]

% Status   : Unsatisfiable
% Rating   : 0.00 v4.1.0, 0.20 v4.0.1, 0.00 v2.1.0
% Syntax   : Number of clauses     :   63 (  36 non-Horn;   0 unit;  63 RR)
%            Number of atoms       :  214 (   0 equality)
%            Maximal clause size   :   10 (   3 average)
%            Number of predicates  :   10 (  10 propositional; 0-0 arity)
%            Number of functors    :    0 (   0 constant; --- arity)
%            Number of variables   :    0 (   0 singleton)
%            Maximal term depth    :    0 (   0 average)
% SPC      : CNF_UNS_PRP

% Comments :
% Bugfixes : v1.1.2 - Renamed from PUZ030-0.
%--------------------------------------------------------------------------
cnf(c1,hypothesis,
    ( ~ salt_mill
    | mustard_barry
    | mustard_lang )).

cnf(c2,hypothesis,
    ( ~ salt_mill
    | mustard_barry
    | salt_lang )).

cnf(c3,hypothesis,
    ( ~ salt_mill
    | salt_barry
    | mustard_lang )).

cnf(c4,hypothesis,
    ( ~ salt_mill
    | salt_barry
    | salt_lang )).

cnf(c5,hypothesis,
    ( ~ mustard_lang
    | ~ mustard_cole
    | ~ mustard_mill )).

cnf(c6,hypothesis,
    ( ~ mustard_lang
    | ~ mustard_cole
    | ~ salt_mill )).

cnf(c7,hypothesis,
    ( ~ mustard_lang
    | ~ salt_cole
    | ~ mustard_mill )).

cnf(c8,hypothesis,
    ( ~ mustard_lang
    | ~ salt_cole
    | ~ salt_mill )).

cnf(c9,hypothesis,
    ( ~ mustard_dix
    | ~ mustard_lang
    | ~ mustard_mill )).

cnf(c10,hypothesis,
    ( ~ mustard_dix
    | ~ mustard_lang
    | ~ salt_mill )).

cnf(c11,hypothesis,
    ( ~ mustard_dix
    | ~ salt_lang
    | ~ mustard_mill )).

cnf(c12,hypothesis,
    ( ~ mustard_dix
    | ~ salt_lang
    | ~ salt_mill )).

cnf(c13,hypothesis,
    ( ~ salt_dix
    | ~ mustard_barry
    | mustard_cole )).

cnf(c14,hypothesis,
    ( ~ salt_dix
    | ~ mustard_barry
    | salt_cole )).

cnf(c15,hypothesis,
    ( ~ salt_dix
    | ~ salt_barry
    | mustard_cole )).

cnf(c16,hypothesis,
    ( ~ salt_dix
    | ~ salt_barry
    | salt_cole )).

cnf(c17,hypothesis,
    ( ~ mustard_cole
    | mustard_dix
    | mustard_lang )).

cnf(c18,hypothesis,
    ( ~ mustard_cole
    | mustard_dix
    | salt_lang )).

cnf(c19,hypothesis,
    ( ~ mustard_cole
    | salt_dix
    | mustard_lang )).

cnf(c20,hypothesis,
    ( ~ mustard_cole
    | salt_dix
    | salt_lang )).

cnf(c21,hypothesis,
    ( ~ mustard_barry
    | ~ mustard_dix
    | mustard_mill )).

cnf(c22,hypothesis,
    ( ~ mustard_barry
    | ~ mustard_dix
    | salt_mill )).

cnf(c23,hypothesis,
    ( ~ mustard_barry
    | ~ salt_dix
    | mustard_mill )).

cnf(c24,hypothesis,
    ( ~ mustard_barry
    | ~ salt_dix
    | salt_mill )).

cnf(c25,hypothesis,
    ( salt_dix
    | ~ mustard_dix
    | mustard_mill )).

cnf(c26,hypothesis,
    ( ~ salt_dix
    | mustard_dix
    | mustard_mill )).

cnf(c27,hypothesis,
    ( salt_cole
    | ~ mustard_cole
    | mustard_mill )).

cnf(c28,hypothesis,
    ( ~ salt_cole
    | mustard_cole
    | mustard_mill )).

cnf(c29,hypothesis,
    ( ~ salt_lang
    | ~ mustard_lang
    | salt_mill )).

cnf(c30,hypothesis,
    ( ~ salt_barry
    | ~ mustard_barry
    | salt_mill )).

cnf(c31,hypothesis,
    ( salt_dix
    | ~ mustard_dix
    | salt_lang )).

cnf(c32,hypothesis,
    ( ~ salt_dix
    | mustard_dix
    | salt_lang )).

cnf(c33,hypothesis,
    ( salt_barry
    | ~ mustard_barry
    | salt_lang )).

cnf(c34,hypothesis,
    ( ~ salt_barry
    | mustard_barry
    | salt_lang )).

cnf(c35,hypothesis,
    ( ~ salt_cole
    | ~ mustard_cole
    | salt_dix )).

cnf(c36,hypothesis,
    ( ~ salt_lang
    | ~ mustard_lang
    | mustard_cole )).

cnf(c37,hypothesis,
    ( ~ salt_dix
    | ~ mustard_dix
    | mustard_cole )).

cnf(c38,hypothesis,
    ( salt_barry
    | ~ mustard_barry
    | salt_cole )).

cnf(c39,hypothesis,
    ( ~ salt_barry
    | mustard_barry
    | salt_cole )).

cnf(c40,hypothesis,
    ( ~ salt_mill
    | ~ mustard_mill
    | mustard_barry )).

cnf(c41,hypothesis,
    ( salt_lang
    | ~ mustard_lang
    | salt_barry )).

cnf(c42,hypothesis,
    ( ~ salt_lang
    | mustard_lang
    | salt_barry )).

cnf(c43,hypothesis,
    ( salt_cole
    | ~ mustard_cole
    | salt_barry )).

cnf(c44,hypothesis,
    ( ~ salt_cole
    | mustard_cole
    | salt_barry )).

cnf(c45,hypothesis,
    ( ~ salt_cole
    | ~ mustard_barry
    | ~ salt_barry
    | ~ mustard_mill )).

cnf(c46,hypothesis,
    ( ~ salt_cole
    | ~ mustard_barry
    | ~ salt_barry
    | ~ salt_mill )).

cnf(c47,hypothesis,
    ( ~ salt_cole
    | salt_barry
    | mustard_barry
    | ~ mustard_mill )).

cnf(c48,hypothesis,
    ( ~ salt_cole
    | salt_barry
    | mustard_barry
    | ~ salt_mill )).

cnf(c49,hypothesis,
    ( ~ mustard_mill
    | ~ mustard_cole
    | ~ salt_cole
    | ~ mustard_dix
    | ~ salt_dix )).

cnf(c50,hypothesis,
    ( ~ mustard_mill
    | salt_cole
    | mustard_cole
    | salt_dix
    | mustard_dix )).

cnf(c51,hypothesis,
    ( ~ salt_lang
    | ~ mustard_barry
    | ~ salt_barry
    | ~ mustard_dix
    | ~ salt_dix )).

cnf(c52,hypothesis,
    ( ~ salt_lang
    | ~ mustard_barry
    | ~ salt_barry
    | salt_dix
    | mustard_dix )).

cnf(c53,hypothesis,
    ( ~ salt_lang
    | salt_barry
    | mustard_barry
    | ~ mustard_dix
    | ~ salt_dix )).

cnf(c54,hypothesis,
    ( ~ salt_barry
    | ~ mustard_cole
    | ~ salt_cole
    | ~ mustard_lang
    | ~ salt_lang )).

cnf(c55,hypothesis,
    ( ~ salt_barry
    | ~ mustard_cole
    | ~ salt_cole
    | salt_lang
    | mustard_lang )).

cnf(c56,hypothesis,
    ( salt_mill
    | mustard_mill
    | mustard_lang )).

cnf(c57,hypothesis,
    ( salt_cole
    | mustard_cole
    | mustard_lang )).

cnf(c58,hypothesis,
    ( salt_mill
    | mustard_mill
    | mustard_dix )).

cnf(c59,hypothesis,
    ( salt_lang
    | mustard_lang
    | mustard_dix )).

cnf(c60,hypothesis,
    ( salt_barry
    | mustard_barry
    | salt_dix )).

cnf(c61,hypothesis,
    ( salt_mill
    | mustard_mill
    | salt_cole )).

cnf(c62,hypothesis,
    ( salt_dix
    | mustard_dix
    | mustard_barry )).

cnf(prove_who_takes_what,negated_conjecture,
    ( salt_lang
    | ~ mustard_barry
    | ~ salt_barry
    | ~ salt_mill
    | ~ mustard_lang
    | salt_cole
    | mustard_cole
    | salt_dix
    | mustard_dix
    | mustard_mill )).

%--------------------------------------------------------------------------
