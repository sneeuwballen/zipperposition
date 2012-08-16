%------------------------------------------------------------------------------
% File     : PUZ063-2 : TPTP v5.2.0. Released v3.2.0.
% Domain   : Puzzles
% Problem  : Problem about mutilated chessboard problem
% Version  : [Pau06] axioms : Reduced > Especial.
% English  :

% Refs     : [Pau06] Paulson (2006), Email to G. Sutcliffe
% Source   : [Pau06]
% Names    :

% Status   : Unsatisfiable
% Rating   : 0.08 v5.2.0, 0.00 v5.1.0, 0.14 v4.1.0, 0.22 v4.0.1, 0.33 v3.7.0, 0.17 v3.3.0, 0.14 v3.2.0
% Syntax   : Number of clauses     :    3 (   0 non-Horn;   3 unit;   2 RR)
%            Number of atoms       :    3 (   1 equality)
%            Maximal clause size   :    1 (   1 average)
%            Number of predicates  :    2 (   0 propositional; 2-3 arity)
%            Number of functors    :    7 (   4 constant; 0-3 arity)
%            Number of variables   :    2 (   1 singleton)
%            Maximal term depth    :    2 (   2 average)
% SPC      : CNF_UNS_RFO_SEQ_HRN

% Comments : The problems in the [Pau06] collection each have very many axioms,
%            of which only a small selection are required for the refutation.
%            The mission is to find those few axioms, after which a refutation
%            can be quite easily found. This version has only the necessary
%            axioms.
%------------------------------------------------------------------------------
cnf(cls_conjecture_0,negated_conjecture,
    ( c_in(v_u,c_Mutil_Otiling(v_A,t_a),tc_set(t_a)) )).

cnf(cls_conjecture_2,negated_conjecture,
    ( ~ c_in(c_union(c_emptyset,v_u,t_a),c_Mutil_Otiling(v_A,t_a),tc_set(t_a)) )).

cnf(cls_Set_OUn__empty__left_0,axiom,
    ( c_union(c_emptyset,V_y,T_a) = V_y )).

%------------------------------------------------------------------------------
