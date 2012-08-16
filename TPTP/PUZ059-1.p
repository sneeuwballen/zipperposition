%------------------------------------------------------------------------------
% File     : PUZ059-1 : TPTP v5.2.0. Released v3.1.0.
% Domain   : Puzzles
% Problem  : Quo vadis 7 - an unreachable state
% Version  : Especial.
% English  : bb is big block (square, size=4 tiles).
%            s1-s4 : 4 small square blocks, size=1 tile
%            v1-v4: 4 vertical blocks, size= 2 tiles
%            b1: horizontal block, size= 2 tiles
%            e1, e2 are the 2 blank tiles
%            It's a 5x4 playing field to move from the start state to
%            the goal state. This is a simple goal for testing.

% Refs     : [Sut03] Suttner (2003), Email to Geoff Sutcliffe.
% Source   : [Sut03]
% Names    :

% Status   : Satisfiable
% Rating   : 0.86 v5.0.0, 0.88 v4.1.0, 0.86 v4.0.1, 0.71 v4.0.0, 0.75 v3.7.0, 0.88 v3.5.0, 0.86 v3.4.0, 0.83 v3.2.0, 0.80 v3.1.0
% Syntax   : Number of clauses     :   43 (   0 non-Horn;   2 unit;  43 RR)
%            Number of atoms       :   84 (   0 equality)
%            Maximal clause size   :    2 (   2 average)
%            Number of predicates  :    1 (   0 propositional; 12-12 arity)
%            Number of functors    :   14 (   1 constant; 0-2 arity)
%            Number of variables   :  480 (   0 singleton)
%            Maximal term depth    :    6 (   1 average)
% SPC      : CNF_SAT_RFO_NEQ

% Comments :
%------------------------------------------------------------------------------
%----Include axioms for quo vadis
include('Axioms/PUZ004-0.ax').
%------------------------------------------------------------------------------
cnf(initial_state,hypothesis,
    ( state(bb(o,s(o)),v1(o,o),v2(o,s(s(s(o)))),v3(s(s(o)),o),v4(s(s(o)),s(s(s(o)))),h(s(s(o)),s(o)),s1(s(s(s(o))),s(o)),s2(s(s(s(o))),s(s(o))),s3(s(s(s(s(o)))),s(o)),s4(s(s(s(s(o)))),s(s(o))),e1(s(s(s(s(o)))),o),e2(s(s(s(s(o)))),s(s(s(o))))) )).

cnf(goal_state,negated_conjecture,
    ( ~ state(bb(o,s(o)),v1(o,o),v2(s(s(s(o))),o),v3(s(o),s(s(s(o)))),v4(s(s(s(s(o)))),s(s(s(o)))),h(s(s(s(s(o)))),s(s(o))),s1(s(s(o)),o),s2(s(s(o)),s(s(o))),s3(o,s(s(s(o)))),s4(s(s(s(s(o)))),s(o)),e1(s(s(o)),s(o)),e2(s(s(s(o))),s(o))) )).

%------------------------------------------------------------------------------
