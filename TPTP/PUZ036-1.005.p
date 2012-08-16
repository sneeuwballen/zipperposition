%--------------------------------------------------------------------------
% File     : PUZ036-1.005 : TPTP v5.2.0. Released v2.0.0.
% Domain   : Puzzles
% Problem  : TopSpin
% Version  : Especial.
%            Theorem formulation : Reverse the first SIZE pieces.
% English  : TopSpin consists of a circular track with 20 pieces numbered
%            1..20 placed in the track, with a turnstile in the track that
%            always holds four consecutive pieces. There are three legal
%            moves in TopSpin: slide all the pieces round the track in
%            either direction, or flip the turnstile. Given any initial
%            board with scrambled pieces on the track, the problem is to
%            find a sequence of moves that unscrambles the pieces.

% Refs     : [Hua96] Huang (1996)Using OTTER and Prolog to Solve TopSpin
% Source   : [Hua96]
% Names    : TopSpin [Hua96]

% Status   : Unsatisfiable
% Rating   : 0.33 v5.0.0, 0.17 v4.1.0, 0.20 v3.7.0, 0.25 v3.5.0, 0.00 v3.3.0, 0.33 v3.2.0, 0.00 v3.1.0, 0.22 v2.7.0, 0.00 v2.6.0, 0.11 v2.5.0, 0.25 v2.4.0, 0.00 v2.1.0
% Syntax   : Number of clauses     :    5 (   0 non-Horn;   2 unit;   5 RR)
%            Number of atoms       :    8 (   0 equality)
%            Maximal clause size   :    2 (   2 average)
%            Number of predicates  :    1 (   0 propositional; 20-20 arity)
%            Number of functors    :   20 (  20 constant; 0-0 arity)
%            Number of variables   :   60 (   0 singleton)
%            Maximal term depth    :    1 (   1 average)
% SPC      : CNF_UNS_EPR

% Comments : Could do lots of other permutations of the pieces.
%          : Increasing size does not correspond to increasing difficulty.
%          : tptp2X: -f tptp -s5 PUZ036-1.g
%--------------------------------------------------------------------------
cnf(make_like_this,negated_conjecture,
    ( ~ state(p_1,p_2,p_3,p_4,p_5,p_6,p_7,p_8,p_9,p_10,p_11,p_12,p_13,p_14,p_15,p_16,p_17,p_18,p_19,p_20) )).

cnf(initial_configuration,hypothesis,
    ( state(p_5,p_4,p_3,p_2,p_1,p_6,p_7,p_8,p_9,p_10,p_11,p_12,p_13,p_14,p_15,p_16,p_17,p_18,p_19,p_20) )).

cnf(move_left,axiom,
    ( ~ state(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20)
    | state(X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X1) )).

cnf(move_right,axiom,
    ( ~ state(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20)
    | state(X20,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19) )).

cnf(flip,axiom,
    ( ~ state(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20)
    | state(X4,X3,X2,X1,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20) )).

%--------------------------------------------------------------------------
