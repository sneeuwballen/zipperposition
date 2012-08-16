%------------------------------------------------------------------------------
% File     : PUZ062-2 : TPTP v5.2.0. Released v3.2.0.
% Domain   : Puzzles
% Problem  : Problem about mutilated chessboard problem
% Version  : [Pau06] axioms : Reduced > Especial.
% English  :

% Refs     : [Pau06] Paulson (2006), Email to G. Sutcliffe
% Source   : [Pau06]
% Names    :

% Status   : Unsatisfiable
% Rating   : 0.83 v5.2.0, 0.75 v5.1.0, 0.76 v5.0.0, 0.64 v4.1.0, 0.62 v4.0.1, 0.64 v3.7.0, 0.50 v3.5.0, 0.55 v3.4.0, 0.58 v3.3.0, 0.57 v3.2.0
% Syntax   : Number of clauses     :   13 (   1 non-Horn;   8 unit;  10 RR)
%            Number of atoms       :   20 (   6 equality)
%            Maximal clause size   :    3 (   2 average)
%            Number of predicates  :    2 (   0 propositional; 2-3 arity)
%            Number of functors    :   20 (  11 constant; 0-4 arity)
%            Number of variables   :   20 (   5 singleton)
%            Maximal term depth    :    6 (   2 average)
% SPC      : CNF_UNS_RFO_SEQ_NHN

% Comments : The problems in the [Pau06] collection each have very many axioms,
%            of which only a small selection are required for the refutation.
%            The mission is to find those few axioms, after which a refutation
%            can be quite easily found. This version has only the necessary
%            axioms.
%------------------------------------------------------------------------------
cnf(cls_Finite__Set_Ocard__insert__disjoint_0,axiom,
    ( ~ c_in(V_A,c_Finite__Set_OFinites,tc_set(T_a))
    | c_in(V_x,V_A,T_a)
    | c_Finite__Set_Ocard(c_insert(V_x,V_A,T_a),T_a) = c_Suc(c_Finite__Set_Ocard(V_A,T_a)) )).

cnf(cls_Finite__Set_Ofinite__Int_1,axiom,
    ( ~ c_in(V_G,c_Finite__Set_OFinites,tc_set(T_a))
    | c_in(c_inter(V_F,V_G,T_a),c_Finite__Set_OFinites,tc_set(T_a)) )).

cnf(cls_Mutil_Otiling__domino__finite_0,axiom,
    ( ~ c_in(V_t,c_Mutil_Otiling(c_Mutil_Odomino,tc_prod(tc_nat,tc_nat)),tc_set(tc_prod(tc_nat,tc_nat)))
    | c_in(V_t,c_Finite__Set_OFinites,tc_set(tc_prod(tc_nat,tc_nat))) )).

cnf(cls_Set_OInt__iff_1,axiom,
    ( ~ c_in(V_c,c_inter(V_A,V_B,T_a),T_a)
    | c_in(V_c,V_B,T_a) )).

cnf(cls_Set_OInt__iff_2,axiom,
    ( ~ c_in(V_c,V_B,T_a)
    | ~ c_in(V_c,V_A,T_a)
    | c_in(V_c,c_inter(V_A,V_B,T_a),T_a) )).

cnf(cls_Set_Oinsert__iff_1,axiom,
    ( c_in(V_x,c_insert(V_x,V_A,T_a),T_a) )).

cnf(cls_Set_Oempty__iff_0,axiom,
    ( ~ c_in(V_c,c_emptyset,T_a) )).

cnf(cls_conjecture_0,negated_conjecture,
    ( c_in(v_t,c_Mutil_Otiling(c_Mutil_Odomino,tc_prod(tc_nat,tc_nat)),tc_set(tc_prod(tc_nat,tc_nat))) )).

cnf(cls_conjecture_1,negated_conjecture,
    ( c_Finite__Set_Ocard(c_inter(c_Mutil_Ocoloured(c_0),v_t,tc_prod(tc_nat,tc_nat)),tc_prod(tc_nat,tc_nat)) = c_Finite__Set_Ocard(c_inter(c_Mutil_Ocoloured(c_Suc(c_0)),v_t,tc_prod(tc_nat,tc_nat)),tc_prod(tc_nat,tc_nat)) )).

cnf(cls_conjecture_2,negated_conjecture,
    ( c_inter(v_a,v_t,tc_prod(tc_nat,tc_nat)) = c_emptyset )).

cnf(cls_conjecture_3,negated_conjecture,
    ( c_inter(c_Mutil_Ocoloured(c_0),v_a,tc_prod(tc_nat,tc_nat)) = c_insert(c_Pair(v_i,v_j,tc_nat,tc_nat),c_emptyset,tc_prod(tc_nat,tc_nat)) )).

cnf(cls_conjecture_4,negated_conjecture,
    ( c_inter(c_Mutil_Ocoloured(c_Suc(c_0)),v_a,tc_prod(tc_nat,tc_nat)) = c_insert(c_Pair(v_m,v_n,tc_nat,tc_nat),c_emptyset,tc_prod(tc_nat,tc_nat)) )).

cnf(cls_conjecture_5,negated_conjecture,
    ( c_Finite__Set_Ocard(c_insert(c_Pair(v_i,v_j,tc_nat,tc_nat),c_inter(c_Mutil_Ocoloured(c_0),v_t,tc_prod(tc_nat,tc_nat)),tc_prod(tc_nat,tc_nat)),tc_prod(tc_nat,tc_nat)) != c_Finite__Set_Ocard(c_insert(c_Pair(v_m,v_n,tc_nat,tc_nat),c_inter(c_Mutil_Ocoloured(c_Suc(c_0)),v_t,tc_prod(tc_nat,tc_nat)),tc_prod(tc_nat,tc_nat)),tc_prod(tc_nat,tc_nat)) )).

%------------------------------------------------------------------------------
