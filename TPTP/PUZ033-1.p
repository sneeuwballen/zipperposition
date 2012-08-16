%--------------------------------------------------------------------------
% File     : PUZ033-1 : TPTP v5.2.0. Released v1.0.0.
% Domain   : Puzzles
% Problem  : The Winds and the Windows Puzzle
% Version  : Especial.
% English  : (1) There is always sunshine when the wind is in the East.
%            (2) When it is cold and foggy, my neighbor practices the flute.
%            (3) When my fire smokes, I set the door open.
%            (4) When it is cold and I feel rheumatic, I light my fire.
%            (5) When the wind is in the East and comes in gusts, my fire
%                smokes.
%            (6) When I keep the door open, I am free from headache.
%            (7) Even when the sun is shining and it is not cold, I keep my
%                window shut if it is foggy.
%            (8) When the wind does not come in gusts, and when I have a
%                fire and keep the door shut, I do not feel rheumatic.
%            (9) Sunshine always brings on fog.
%            (10) When my neighbor practices the flute, I shut the door,
%                even if I have no headache.
%            (11) When there is a fog and the wind is in the East, I feel
%                rheumatic.
%            Show that when the wind is in the East, I keep my windows shut.

% Refs     : [Car86] Carroll (1986), Lewis Carroll's Symbolic Logic
% Source   : [ANL]
% Names    : winds.ver1.in [ANL]

% Status   : Unsatisfiable
% Rating   : 0.00 v2.0.0
% Syntax   : Number of clauses     :   13 (   2 non-Horn;   2 unit;  13 RR)
%            Number of atoms       :   32 (   0 equality)
%            Maximal clause size   :    4 (   2 average)
%            Number of predicates  :   12 (  12 propositional; 0-0 arity)
%            Number of functors    :    0 (   0 constant; --- arity)
%            Number of variables   :    0 (   0 singleton)
%            Maximal term depth    :    0 (   0 average)
% SPC      : CNF_UNS_PRP

% Comments :
%--------------------------------------------------------------------------
cnf(c1,axiom,
    ( ~ wind_in_east
    | sunshine )).

cnf(c2,axiom,
    ( ~ cold
    | ~ foggy
    | neighbor_practices_flute )).

cnf(c3,axiom,
    ( ~ fire_smokes
    | door_is_open )).

cnf(c4,axiom,
    ( ~ cold
    | ~ i_feel_rheumatic
    | fire_is_lit )).

cnf(c5,axiom,
    ( ~ wind_in_east
    | ~ wind_in_gusts
    | fire_smokes )).

cnf(c6,axiom,
    ( ~ door_is_open
    | ~ headache )).

cnf(c7,axiom,
    ( ~ sunshine
    | cold
    | ~ foggy
    | window_is_shut )).

cnf(c8,axiom,
    ( wind_in_gusts
    | ~ fire_is_lit
    | door_is_open
    | ~ i_feel_rheumatic )).

cnf(c9,axiom,
    ( ~ sunshine
    | foggy )).

cnf(c10,axiom,
    ( ~ neighbor_practices_flute
    | ~ door_is_open )).

cnf(c11,axiom,
    ( ~ foggy
    | ~ wind_in_east
    | i_feel_rheumatic )).

cnf(c12,hypothesis,
    ( wind_in_east )).

cnf(prove_window_is_shut,negated_conjecture,
    ( ~ window_is_shut )).

%--------------------------------------------------------------------------
