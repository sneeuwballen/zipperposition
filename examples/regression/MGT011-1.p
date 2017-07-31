%--------------------------------------------------------------------------
% File     : MGT011-1 : TPTP v6.2.0. Released v2.4.0.
% Domain   : Management (Organisation Theory)
% Problem  : Organizational size cannot decrease without reorganization
% Version  : [PB+94] axioms.
% English  :

% Refs     : [PB+92] Peli et al. (1992), A Logical Approach to Formalizing
%          : [PB+94] Peli et al. (1994), A Logical Approach to Formalizing
%          : [Kam94] Kamps (1994), Email to G. Sutcliffe
% Source   : [TPTP]
% Names    :

% Status   : Unsatisfiable
% Rating   : 0.00 v6.1.0, 0.20 v6.0.0, 0.22 v5.5.0, 0.12 v5.4.0, 0.13 v5.3.0, 0.17 v5.2.0, 0.25 v5.1.0, 0.29 v5.0.0, 0.14 v4.1.0, 0.11 v4.0.1, 0.17 v4.0.0, 0.33 v3.5.0, 0.17 v3.3.0, 0.00 v3.1.0, 0.11 v2.7.0, 0.00 v2.4.0
% Syntax   : Number of clauses     :   14 (   0 non-Horn;   7 unit;  14 RR)
%            Number of atoms       :   38 (   2 equality)
%            Maximal clause size   :   10 (   3 average)
%            Number of predicates  :    7 (   0 propositional; 2-3 arity)
%            Number of functors    :    7 (   5 constant; 0-2 arity)
%            Number of variables   :   27 (   0 singleton)
%            Maximal term depth    :    2 (   1 average)
% SPC      : CNF_UNS_RFO_SEQ_HRN

% Comments : Created with tptp2X -f tptp -t clausify:otter MGT011+1.p
%--------------------------------------------------------------------------
cnf(mp5_20,axiom,
    ( ~ organization(A,B)
    | inertia(A,sk1(B,A),B) )).

cnf(mp6_1_21,axiom,
    ( ~ greater(A,B)
    | A != B )).

cnf(mp6_2_22,axiom,
    ( ~ greater(A,B)
    | ~ greater(B,A) )).

cnf(mp9_23,axiom,
    ( ~ organization(A,B)
    | class(A,sk2(B,A),B) )).

cnf(mp10_24,axiom,
    ( ~ organization(A,B)
    | ~ organization(A,C)
    | ~ reorganization_free(A,B,C)
    | ~ class(A,D,B)
    | ~ class(A,E,C)
    | D = E )).

cnf(a5_FOL_25,hypothesis,
    ( ~ organization(A,B)
    | ~ organization(C,D)
    | ~ class(A,E,B)
    | ~ class(C,E,D)
    | ~ size(A,F,B)
    | ~ size(C,G,D)
    | ~ inertia(A,H,B)
    | ~ inertia(C,I,D)
    | ~ greater(G,F)
    | greater(I,H) )).

cnf(t2_FOL_26,hypothesis,
    ( ~ organization(A,B)
    | ~ organization(A,C)
    | ~ reorganization_free(A,B,C)
    | ~ inertia(A,D,B)
    | ~ inertia(A,E,C)
    | ~ greater(C,B)
    | greater(E,D) )).

cnf(t11_FOL_27,negated_conjecture,
    ( organization(sk3,sk6) )).

cnf(t11_FOL_28,negated_conjecture,
    ( organization(sk3,sk7) )).

cnf(t11_FOL_29,negated_conjecture,
    ( reorganization_free(sk3,sk6,sk7) )).

cnf(t11_FOL_30,negated_conjecture,
    ( size(sk3,sk4,sk6) )).

cnf(t11_FOL_31,negated_conjecture,
    ( size(sk3,sk5,sk7) )).

cnf(t11_FOL_32,negated_conjecture,
    ( greater(sk7,sk6) )).

cnf(t11_FOL_33,negated_conjecture,
    ( greater(sk4,sk5) )).

%--------------------------------------------------------------------------
