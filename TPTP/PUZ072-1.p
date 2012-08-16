%------------------------------------------------------------------------------
% File     : PUZ072-1 : TPTP v5.2.0. Released v3.2.0.
% Domain   : Puzzles (Sudoku)
% Problem  : Friday's Sudoku
% Version  : [Bau06] axioms : Especial.
% English  :  1      9
%               3  8
%                  6
%                124
%            7 3
%            5
%            8  6
%                4  2
%               7   5

% Refs     : [Bau06] Baumgartner (2006), Email to G. Sutcliffe
% Source   : [Bau06]
% Names    :

% Status   : Satisfiable
% Rating   : 0.78 v5.2.0, 0.70 v5.0.0, 0.67 v4.1.0, 0.57 v4.0.1, 0.60 v4.0.0, 0.25 v3.7.0, 0.33 v3.4.0, 0.50 v3.3.0, 0.33 v3.2.0
% Syntax   : Number of clauses     :   96 (   4 non-Horn;  71 unit;  96 RR)
%            Number of atoms       :  178 (  39 equality)
%            Maximal clause size   :   11 (   2 average)
%            Number of predicates  :    4 (   0 propositional; 1-3 arity)
%            Number of functors    :    2 (   1 constant; 0-1 arity)
%            Number of variables   :   75 (   0 singleton)
%            Maximal term depth    :   10 (   3 average)
% SPC      : CNF_SAT_RFO_EQU_NUE

% Comments :
%------------------------------------------------------------------------------
include('Axioms/PUZ005-0.ax').
%------------------------------------------------------------------------------
cnf(c01,axiom,
    ( el(s(n0),s(s(n0)),s(n0)) )).

cnf(c02,axiom,
    ( el(s(n0),s(s(s(s(s(s(s(s(s(n0))))))))),s(s(s(s(s(s(s(s(s(n0)))))))))) )).

cnf(c03,axiom,
    ( el(s(s(n0)),s(s(s(s(n0)))),s(s(s(n0)))) )).

cnf(c04,axiom,
    ( el(s(s(n0)),s(s(s(s(s(s(s(n0))))))),s(s(s(s(s(s(s(s(n0))))))))) )).

cnf(c05,axiom,
    ( el(s(s(s(n0))),s(s(s(s(s(s(s(n0))))))),s(s(s(s(s(s(n0))))))) )).

cnf(c06,axiom,
    ( el(s(s(s(s(n0)))),s(s(s(s(s(n0))))),s(n0)) )).

cnf(c07,axiom,
    ( el(s(s(s(s(n0)))),s(s(s(s(s(s(n0)))))),s(s(n0))) )).

cnf(c08,axiom,
    ( el(s(s(s(s(n0)))),s(s(s(s(s(s(s(n0))))))),s(s(s(s(n0))))) )).

cnf(c09,axiom,
    ( el(s(s(s(s(s(n0))))),s(n0),s(s(s(s(s(s(s(n0)))))))) )).

cnf(c10,axiom,
    ( el(s(s(s(s(s(n0))))),s(s(s(n0))),s(s(s(n0)))) )).

cnf(c11,axiom,
    ( el(s(s(s(s(s(s(n0)))))),s(n0),s(s(s(s(s(n0)))))) )).

cnf(c12,axiom,
    ( el(s(s(s(s(s(s(s(n0))))))),s(n0),s(s(s(s(s(s(s(s(n0))))))))) )).

cnf(c13,axiom,
    ( el(s(s(s(s(s(s(s(n0))))))),s(s(s(s(n0)))),s(s(s(s(s(s(n0))))))) )).

cnf(c14,axiom,
    ( el(s(s(s(s(s(s(s(s(n0)))))))),s(s(s(s(s(n0))))),s(s(s(s(n0))))) )).

cnf(c15,axiom,
    ( el(s(s(s(s(s(s(s(s(n0)))))))),s(s(s(s(s(s(s(s(n0)))))))),s(s(n0))) )).

cnf(c16,axiom,
    ( el(s(s(s(s(s(s(s(s(s(n0))))))))),s(s(s(s(n0)))),s(s(s(s(s(s(s(n0)))))))) )).

cnf(c17,axiom,
    ( el(s(s(s(s(s(s(s(s(s(n0))))))))),s(s(s(s(s(s(s(s(n0)))))))),s(s(s(s(s(n0)))))) )).

%------------------------------------------------------------------------------
