%--------------------------------------------------------------------------
% File     : PUZ015-3 : TPTP v5.2.0. Released v2.7.0.
% Domain   : Puzzles
% Problem  : Checkerboard and Dominoes : Corners removed
% Version  : [Cla03] axioms : Especial.
% English  : There is a checker board where two opposite corners are removed.
%            There is a box of dominoes that are one square by two squares
%            in size. Can you exactly cover the checker board with dominoes?
%            The answer is "no", which is shown by the satisfiability of
%            this problem.

% Refs     : [Cla03] Claessen (2003), Email to G. Sutcliffe
% Source   : [Cla03]
% Names    :

% Status   : Satisfiable
% Rating   : 0.86 v5.0.0, 0.88 v4.1.0, 0.86 v4.0.0, 0.88 v3.5.0, 0.86 v3.4.0, 0.83 v3.2.0, 0.80 v3.1.0, 0.86 v2.7.0
% Syntax   : Number of clauses     :   32 (   0 non-Horn;  17 unit;  17 RR)
%            Number of atoms       :   62 (   0 equality)
%            Maximal clause size   :    3 (   2 average)
%            Number of predicates  :    3 (   0 propositional; 2-8 arity)
%            Number of functors    :    3 (   2 constant; 0-8 arity)
%            Number of variables   :  296 (   0 singleton)
%            Maximal term depth    :    2 (   1 average)
% SPC      : CNF_SAT_RFO_NEQ

% Comments :
%--------------------------------------------------------------------------
cnf(initial_state,axiom,
    ( state(row(d,o,o,o,o,o,o,o),row(o,o,o,o,o,o,o,o),row(o,o,o,o,o,o,o,o),row(o,o,o,o,o,o,o,o),row(o,o,o,o,o,o,o,o),row(o,o,o,o,o,o,o,o),row(o,o,o,o,o,o,o,o),row(o,o,o,o,o,o,o,d)) )).

cnf(final_state,negated_conjecture,
    ( ~ state(row(d,d,d,d,d,d,d,d),row(d,d,d,d,d,d,d,d),row(d,d,d,d,d,d,d,d),row(d,d,d,d,d,d,d,d),row(d,d,d,d,d,d,d,d),row(d,d,d,d,d,d,d,d),row(d,d,d,d,d,d,d,d),row(d,d,d,d,d,d,d,d)) )).

cnf(hori_1,axiom,
    ( horizontal(row(o,o,C3,C4,C5,C6,C7,C8),row(d,d,C3,C4,C5,C6,C7,C8)) )).

cnf(hori_2,axiom,
    ( horizontal(row(C1,o,o,C4,C5,C6,C7,C8),row(C1,d,d,C4,C5,C6,C7,C8)) )).

cnf(hori_3,axiom,
    ( horizontal(row(C1,C2,o,o,C5,C6,C7,C8),row(C1,C2,d,d,C5,C6,C7,C8)) )).

cnf(hori_4,axiom,
    ( horizontal(row(C1,C2,C3,o,o,C6,C7,C8),row(C1,C2,C3,d,d,C6,C7,C8)) )).

cnf(hori_5,axiom,
    ( horizontal(row(C1,C2,C3,C4,o,o,C7,C8),row(C1,C2,C3,C4,d,d,C7,C8)) )).

cnf(hori_6,axiom,
    ( horizontal(row(C1,C2,C3,C4,C5,o,o,C8),row(C1,C2,C3,C4,C5,d,d,C8)) )).

cnf(hori_7,axiom,
    ( horizontal(row(C1,C2,C3,C4,C5,C6,o,o),row(C1,C2,C3,C4,C5,C6,d,d)) )).

cnf(hori_move_1,axiom,
    ( state(RB,R2,R3,R4,R5,R6,R7,R8)
    | ~ state(RA,R2,R3,R4,R5,R6,R7,R8)
    | ~ horizontal(RA,RB) )).

cnf(hori_move_2,axiom,
    ( state(R1,RB,R3,R4,R5,R6,R7,R8)
    | ~ state(R1,RA,R3,R4,R5,R6,R7,R8)
    | ~ horizontal(RA,RB) )).

cnf(hori_move_3,axiom,
    ( state(R1,R2,RB,R4,R5,R6,R7,R8)
    | ~ state(R1,R2,RA,R4,R5,R6,R7,R8)
    | ~ horizontal(RA,RB) )).

cnf(hori_move_4,axiom,
    ( state(R1,R2,R3,RB,R5,R6,R7,R8)
    | ~ state(R1,R2,R3,RA,R5,R6,R7,R8)
    | ~ horizontal(RA,RB) )).

cnf(hori_move_5,axiom,
    ( state(R1,R2,R3,R4,RB,R6,R7,R8)
    | ~ state(R1,R2,R3,R4,RA,R6,R7,R8)
    | ~ horizontal(RA,RB) )).

cnf(hori_move_6,axiom,
    ( state(R1,R2,R3,R4,R5,RB,R7,R8)
    | ~ state(R1,R2,R3,R4,R5,RA,R7,R8)
    | ~ horizontal(RA,RB) )).

cnf(hori_move_7,axiom,
    ( state(R1,R2,R3,R4,R5,R6,RB,R8)
    | ~ state(R1,R2,R3,R4,R5,R6,RA,R8)
    | ~ horizontal(RA,RB) )).

cnf(hori_move_8,axiom,
    ( state(R1,R2,R3,R4,R5,R6,R7,RB)
    | ~ state(R1,R2,R3,R4,R5,R6,R7,RA)
    | ~ horizontal(RA,RB) )).

cnf(verti_1,axiom,
    ( vertical(row(o,C2,C3,C4,C5,C6,C7,C8),row(o,D2,D3,D4,D5,D6,D7,D8),row(d,C2,C3,C4,C5,C6,C7,C8),row(d,D2,D3,D4,D5,D6,D7,D8)) )).

cnf(verti_2,axiom,
    ( vertical(row(C1,o,C3,C4,C5,C6,C7,C8),row(D1,o,D3,D4,D5,D6,D7,D8),row(C1,d,C3,C4,C5,C6,C7,C8),row(D1,d,D3,D4,D5,D6,D7,D8)) )).

cnf(verti_3,axiom,
    ( vertical(row(C1,C2,o,C4,C5,C6,C7,C8),row(D1,D2,o,D4,D5,D6,D7,D8),row(C1,C2,d,C4,C5,C6,C7,C8),row(D1,D2,d,D4,D5,D6,D7,D8)) )).

cnf(verti_4,axiom,
    ( vertical(row(C1,C2,C3,o,C5,C6,C7,C8),row(D1,D2,D3,o,D5,D6,D7,D8),row(C1,C2,C3,d,C5,C6,C7,C8),row(D1,D2,D3,d,D5,D6,D7,D8)) )).

cnf(verti_5,axiom,
    ( vertical(row(C1,C2,C3,C4,o,C6,C7,C8),row(D1,D2,D3,D4,o,D6,D7,D8),row(C1,C2,C3,C4,d,C6,C7,C8),row(D1,D2,D3,D4,d,D6,D7,D8)) )).

cnf(verti_6,axiom,
    ( vertical(row(C1,C2,C3,C4,C5,o,C7,C8),row(D1,D2,D3,D4,D5,o,D7,D8),row(C1,C2,C3,C4,C5,d,C7,C8),row(D1,D2,D3,D4,D5,d,D7,D8)) )).

cnf(verti_7,axiom,
    ( vertical(row(C1,C2,C3,C4,C5,C6,o,C8),row(D1,D2,D3,D4,D5,D6,o,D8),row(C1,C2,C3,C4,C5,C6,d,C8),row(D1,D2,D3,D4,D5,D6,d,D8)) )).

cnf(verti_8,axiom,
    ( vertical(row(C1,C2,C3,C4,C5,C6,C7,o),row(D1,D2,D3,D4,D5,D6,D7,o),row(C1,C2,C3,C4,C5,C6,C7,d),row(D1,D2,D3,D4,D5,D6,D7,d)) )).

cnf(verti_move_1_2,axiom,
    ( state(RC,RD,R3,R4,R5,R6,R7,R8)
    | ~ state(RA,RB,R3,R4,R5,R6,R7,R8)
    | ~ vertical(RA,RB,RC,RD) )).

cnf(verti_move_2_3,axiom,
    ( state(R1,RC,RD,R4,R5,R6,R7,R8)
    | ~ state(R1,RA,RB,R4,R5,R6,R7,R8)
    | ~ vertical(RA,RB,RC,RD) )).

cnf(verti_move_3_4,axiom,
    ( state(R1,R2,RC,RD,R5,R6,R7,R8)
    | ~ state(R1,R2,RA,RB,R5,R6,R7,R8)
    | ~ vertical(RA,RB,RC,RD) )).

cnf(verti_move_4_5,axiom,
    ( state(R1,R2,R3,RC,RD,R6,R7,R8)
    | ~ state(R1,R2,R3,RA,RB,R6,R7,R8)
    | ~ vertical(RA,RB,RC,RD) )).

cnf(verti_move_5_6,axiom,
    ( state(R1,R2,R3,R4,RC,RD,R7,R8)
    | ~ state(R1,R2,R3,R4,RA,RB,R7,R8)
    | ~ vertical(RA,RB,RC,RD) )).

cnf(verti_move_6_7,axiom,
    ( state(R1,R2,R3,R4,R5,RC,RD,R8)
    | ~ state(R1,R2,R3,R4,R5,RA,RB,R8)
    | ~ vertical(RA,RB,RC,RD) )).

cnf(verti_move_7_8,axiom,
    ( state(R1,R2,R3,R4,R5,R6,RC,RD)
    | ~ state(R1,R2,R3,R4,R5,R6,RA,RB)
    | ~ vertical(RA,RB,RC,RD) )).

%--------------------------------------------------------------------------
