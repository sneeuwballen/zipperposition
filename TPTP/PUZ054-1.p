%--------------------------------------------------------------------------
% File     : PUZ054-1 : TPTP v5.2.0. Released v2.7.0.
% Domain   : Puzzles
% Problem  : Take black and white balls from a bag
% Version  : [Cla03] axioms : Especial.
% English  : Start with a bag with 10 white balls and 9 black balls.
%            Take out two balls: if they have the same color, put a black
%            ball back; if they have a different color, put a white ball back.
%            The last ball left cannot be white.

% Refs     : [Cla03] Claessen (2003), Email to G. Sutcliffe
% Source   : [Cla03]
% Names    :

% Status   : Satisfiable
% Rating   : 0.00 v2.7.0
% Syntax   : Number of clauses     :    5 (   0 non-Horn;   2 unit;   5 RR)
%            Number of atoms       :    8 (   0 equality)
%            Maximal clause size   :    2 (   2 average)
%            Number of predicates  :    1 (   0 propositional; 2-2 arity)
%            Number of functors    :    2 (   1 constant; 0-1 arity)
%            Number of variables   :    6 (   0 singleton)
%            Maximal term depth    :   11 (   3 average)
% SPC      : CNF_SAT_RFO_NEQ

% Comments :
%--------------------------------------------------------------------------
cnf(initial_state,axiom,
    ( p(s(s(s(s(s(s(s(s(s(s(n0)))))))))),s(s(s(s(s(s(s(s(s(n0)))))))))) )).

cnf(two_whites_out_one_black_in,axiom,
    ( p(X,s(Y))
    | ~ p(s(s(X)),Y) )).

cnf(two_blacks_out_one_black_in,axiom,
    ( p(X,s(Y))
    | ~ p(X,s(s(Y))) )).

cnf(two_different_balls_out_one_white_in,axiom,
    ( p(s(X),Y)
    | ~ p(s(X),s(Y)) )).

cnf(goal_state,negated_conjecture,
    ( ~ p(s(n0),n0) )).

%--------------------------------------------------------------------------
