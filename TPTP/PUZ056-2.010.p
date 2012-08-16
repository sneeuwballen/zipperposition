%------------------------------------------------------------------------------
% File     : PUZ056-2.010 : TPTP v5.2.0. Released v3.5.0.
% Domain   : Puzzles
% Problem  : Towers of Hanoi k=10
% Version  : Especial.
% English  : Each instance encodes Tower of Hanoi with n discs as a
%            reachability problem.

% Refs     : [NV07]  Navarro (2007), Email to Geoff Sutcliffe
% Source   : [NV07]
% Names    : hanoi-k10 [NV07a]

% Status   : Unsatisfiable
% Rating   : 0.00 v5.0.0, 0.17 v4.1.0, 0.20 v3.7.0, 0.25 v3.5.0
% Syntax   : Number of clauses     :   21 (   0 non-Horn;  11 unit;  20 RR)
%            Number of atoms       :  121 (   0 equality)
%            Maximal clause size   :   20 (   6 average)
%            Number of predicates  :    2 (   0 propositional; 2-10 arity)
%            Number of functors    :    3 (   3 constant; 0-0 arity)
%            Number of variables   :  110 (   2 singleton)
%            Maximal term depth    :    1 (   1 average)
% SPC      : CNF_UNS_EPR

% Comments : k >= 13 not solved by any prover in less than 1 hr. (2007)
%------------------------------------------------------------------------------
cnf(rule1,axiom,
    ( ~ p(I,T1,T2,T3,T4,T5,T6,T7,T8,T9)
    | p(J,T1,T2,T3,T4,T5,T6,T7,T8,T9) )).

cnf(rule2,axiom,
    ( ~ p(T0,I,T2,T3,T4,T5,T6,T7,T8,T9)
    | ~ neq(T0,I)
    | ~ neq(T0,J)
    | p(T0,J,T2,T3,T4,T5,T6,T7,T8,T9) )).

cnf(rule3,axiom,
    ( ~ p(T0,T1,I,T3,T4,T5,T6,T7,T8,T9)
    | ~ neq(T0,I)
    | ~ neq(T0,J)
    | ~ neq(T1,I)
    | ~ neq(T1,J)
    | p(T0,T1,J,T3,T4,T5,T6,T7,T8,T9) )).

cnf(rule4,axiom,
    ( ~ p(T0,T1,T2,I,T4,T5,T6,T7,T8,T9)
    | ~ neq(T0,I)
    | ~ neq(T0,J)
    | ~ neq(T1,I)
    | ~ neq(T1,J)
    | ~ neq(T2,I)
    | ~ neq(T2,J)
    | p(T0,T1,T2,J,T4,T5,T6,T7,T8,T9) )).

cnf(rule5,axiom,
    ( ~ p(T0,T1,T2,T3,I,T5,T6,T7,T8,T9)
    | ~ neq(T0,I)
    | ~ neq(T0,J)
    | ~ neq(T1,I)
    | ~ neq(T1,J)
    | ~ neq(T2,I)
    | ~ neq(T2,J)
    | ~ neq(T3,I)
    | ~ neq(T3,J)
    | p(T0,T1,T2,T3,J,T5,T6,T7,T8,T9) )).

cnf(rule6,axiom,
    ( ~ p(T0,T1,T2,T3,T4,I,T6,T7,T8,T9)
    | ~ neq(T0,I)
    | ~ neq(T0,J)
    | ~ neq(T1,I)
    | ~ neq(T1,J)
    | ~ neq(T2,I)
    | ~ neq(T2,J)
    | ~ neq(T3,I)
    | ~ neq(T3,J)
    | ~ neq(T4,I)
    | ~ neq(T4,J)
    | p(T0,T1,T2,T3,T4,J,T6,T7,T8,T9) )).

cnf(rule7,axiom,
    ( ~ p(T0,T1,T2,T3,T4,T5,I,T7,T8,T9)
    | ~ neq(T0,I)
    | ~ neq(T0,J)
    | ~ neq(T1,I)
    | ~ neq(T1,J)
    | ~ neq(T2,I)
    | ~ neq(T2,J)
    | ~ neq(T3,I)
    | ~ neq(T3,J)
    | ~ neq(T4,I)
    | ~ neq(T4,J)
    | ~ neq(T5,I)
    | ~ neq(T5,J)
    | p(T0,T1,T2,T3,T4,T5,J,T7,T8,T9) )).

cnf(rule8,axiom,
    ( ~ p(T0,T1,T2,T3,T4,T5,T6,I,T8,T9)
    | ~ neq(T0,I)
    | ~ neq(T0,J)
    | ~ neq(T1,I)
    | ~ neq(T1,J)
    | ~ neq(T2,I)
    | ~ neq(T2,J)
    | ~ neq(T3,I)
    | ~ neq(T3,J)
    | ~ neq(T4,I)
    | ~ neq(T4,J)
    | ~ neq(T5,I)
    | ~ neq(T5,J)
    | ~ neq(T6,I)
    | ~ neq(T6,J)
    | p(T0,T1,T2,T3,T4,T5,T6,J,T8,T9) )).

cnf(rule9,axiom,
    ( ~ p(T0,T1,T2,T3,T4,T5,T6,T7,I,T9)
    | ~ neq(T0,I)
    | ~ neq(T0,J)
    | ~ neq(T1,I)
    | ~ neq(T1,J)
    | ~ neq(T2,I)
    | ~ neq(T2,J)
    | ~ neq(T3,I)
    | ~ neq(T3,J)
    | ~ neq(T4,I)
    | ~ neq(T4,J)
    | ~ neq(T5,I)
    | ~ neq(T5,J)
    | ~ neq(T6,I)
    | ~ neq(T6,J)
    | ~ neq(T7,I)
    | ~ neq(T7,J)
    | p(T0,T1,T2,T3,T4,T5,T6,T7,J,T9) )).

cnf(rule10,axiom,
    ( ~ p(T0,T1,T2,T3,T4,T5,T6,T7,T8,I)
    | ~ neq(T0,I)
    | ~ neq(T0,J)
    | ~ neq(T1,I)
    | ~ neq(T1,J)
    | ~ neq(T2,I)
    | ~ neq(T2,J)
    | ~ neq(T3,I)
    | ~ neq(T3,J)
    | ~ neq(T4,I)
    | ~ neq(T4,J)
    | ~ neq(T5,I)
    | ~ neq(T5,J)
    | ~ neq(T6,I)
    | ~ neq(T6,J)
    | ~ neq(T7,I)
    | ~ neq(T7,J)
    | ~ neq(T8,I)
    | ~ neq(T8,J)
    | p(T0,T1,T2,T3,T4,T5,T6,T7,T8,J) )).

cnf(neq1,axiom,
    ( ~ neq(s0,s0) )).

cnf(neq2,axiom,
    ( neq(s0,s1) )).

cnf(neq3,axiom,
    ( neq(s0,s2) )).

cnf(neq4,axiom,
    ( neq(s1,s0) )).

cnf(neq5,axiom,
    ( ~ neq(s1,s1) )).

cnf(neq6,axiom,
    ( neq(s1,s2) )).

cnf(neq7,axiom,
    ( neq(s2,s0) )).

cnf(neq8,axiom,
    ( neq(s2,s1) )).

cnf(neq9,axiom,
    ( ~ neq(s2,s2) )).

cnf(init,axiom,
    ( p(s0,s0,s0,s0,s0,s0,s0,s0,s0,s0) )).

cnf(goal,negated_conjecture,
    ( ~ p(s2,s2,s2,s2,s2,s2,s2,s2,s2,s2) )).

%------------------------------------------------------------------------------
