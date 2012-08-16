%------------------------------------------------------------------------------
% File     : PUZ064-2 : TPTP v5.2.0. Released v3.2.0.
% Domain   : Puzzles
% Problem  : Problem about mutilated chessboard problem
% Version  : [Pau06] axioms : Reduced > Especial.
% English  :

% Refs     : [Pau06] Paulson (2006), Email to G. Sutcliffe
% Source   : [Pau06]
% Names    :

% Status   : Unsatisfiable
% Rating   : 0.78 v5.2.0, 0.69 v5.1.0, 0.76 v5.0.0, 0.71 v4.1.0, 0.77 v4.0.1, 0.73 v3.7.0, 0.50 v3.5.0, 0.55 v3.4.0, 0.67 v3.3.0, 0.64 v3.2.0
% Syntax   : Number of clauses     :   29 (   1 non-Horn;  21 unit;  13 RR)
%            Number of atoms       :   42 (  18 equality)
%            Maximal clause size   :    4 (   1 average)
%            Number of predicates  :    3 (   0 propositional; 2-3 arity)
%            Number of functors    :   16 (   7 constant; 0-4 arity)
%            Number of variables   :   64 (  15 singleton)
%            Maximal term depth    :    4 (   2 average)
% SPC      : CNF_UNS_RFO_SEQ_NHN

% Comments : The problems in the [Pau06] collection each have very many axioms,
%            of which only a small selection are required for the refutation.
%            The mission is to find those few axioms, after which a refutation
%            can be quite easily found. This version has only the necessary
%            axioms.
%------------------------------------------------------------------------------
cnf(cls_conjecture_0,negated_conjecture,
    ( c_in(v_a,v_A,tc_set(t_a)) )).

cnf(cls_conjecture_2,negated_conjecture,
    ( c_inter(v_a,v_t,t_a) = c_emptyset )).

cnf(cls_conjecture_3,negated_conjecture,
    ( c_in(v_u,c_Mutil_Otiling(v_A,t_a),tc_set(t_a)) )).

cnf(cls_conjecture_4,negated_conjecture,
    ( c_inter(c_union(v_a,v_t,t_a),v_u,t_a) = c_emptyset )).

cnf(cls_conjecture_5,negated_conjecture,
    ( ~ c_in(c_union(c_union(v_a,v_t,t_a),v_u,t_a),c_Mutil_Otiling(v_A,t_a),tc_set(t_a)) )).

cnf(cls_conjecture_6,negated_conjecture,
    ( c_in(c_union(v_t,v_u,t_a),c_Mutil_Otiling(v_A,t_a),tc_set(t_a))
    | c_inter(v_t,v_u,t_a) != c_emptyset
    | ~ c_in(v_u,c_Mutil_Otiling(v_A,t_a),tc_set(t_a)) )).

cnf(cls_Mutil_Otiling_OUn_0,axiom,
    ( ~ c_in(V_a,V_A,tc_set(T_a))
    | ~ c_in(V_t,c_Mutil_Otiling(V_A,T_a),tc_set(T_a))
    | c_inter(V_a,V_t,T_a) != c_emptyset
    | c_in(c_union(V_a,V_t,T_a),c_Mutil_Otiling(V_A,T_a),tc_set(T_a)) )).

cnf(cls_Relation_OImage__diag_0,axiom,
    ( c_Relation_OImage(c_Relation_Odiag(V_A,T_a),V_B,T_a,T_a) = c_inter(V_A,V_B,T_a) )).

cnf(cls_Set_OCompl__Diff__eq_0,axiom,
    ( c_uminus(c_minus(V_A,V_B,tc_set(T_a)),tc_set(T_a)) = c_union(c_uminus(V_A,tc_set(T_a)),V_B,T_a) )).

cnf(cls_Set_OCompl__UNIV__eq_0,axiom,
    ( c_uminus(c_UNIV,tc_set(T_a)) = c_emptyset )).

cnf(cls_Set_ODiff__Compl_0,axiom,
    ( c_minus(V_A,c_uminus(V_B,tc_set(T_a)),tc_set(T_a)) = c_inter(V_A,V_B,T_a) )).

cnf(cls_Set_OInt__UNIV__left_0,axiom,
    ( c_inter(c_UNIV,V_y,T_a) = V_y )).

cnf(cls_Set_OInt__iff_1,axiom,
    ( ~ c_in(V_c,c_inter(V_A,V_B,T_a),T_a)
    | c_in(V_c,V_B,T_a) )).

cnf(cls_Set_OUn__Diff__cancel2_0,axiom,
    ( c_union(c_minus(V_B,V_A,tc_set(T_a)),V_A,T_a) = c_union(V_B,V_A,T_a) )).

cnf(cls_Set_OUn__Diff__cancel_0,axiom,
    ( c_union(V_A,c_minus(V_B,V_A,tc_set(T_a)),T_a) = c_union(V_A,V_B,T_a) )).

cnf(cls_Set_OUn__UNIV__left_0,axiom,
    ( c_union(c_UNIV,V_B,T_a) = c_UNIV )).

cnf(cls_Set_OUn__absorb_0,axiom,
    ( c_union(V_y,V_y,T_a) = V_y )).

cnf(cls_Set_OUn__assoc_0,axiom,
    ( c_union(c_union(V_A,V_B,T_a),V_C,T_a) = c_union(V_A,c_union(V_B,V_C,T_a),T_a) )).

cnf(cls_Set_OUn__empty__left_0,axiom,
    ( c_union(c_emptyset,V_y,T_a) = V_y )).

cnf(cls_Set_OUn__empty__right_0,axiom,
    ( c_union(V_y,c_emptyset,T_a) = V_y )).

cnf(cls_Set_OUn__subset__iff_0,axiom,
    ( ~ c_lessequals(c_union(V_A,V_B,T_a),V_C,tc_set(T_a))
    | c_lessequals(V_A,V_C,tc_set(T_a)) )).

cnf(cls_Set_OUn__subset__iff_1,axiom,
    ( ~ c_lessequals(c_union(V_A,V_B,T_a),V_C,tc_set(T_a))
    | c_lessequals(V_B,V_C,tc_set(T_a)) )).

cnf(cls_Set_OUn__subset__iff_2,axiom,
    ( ~ c_lessequals(V_B,V_C,tc_set(T_a))
    | ~ c_lessequals(V_A,V_C,tc_set(T_a))
    | c_lessequals(c_union(V_A,V_B,T_a),V_C,tc_set(T_a)) )).

cnf(cls_Set_Odouble__complement_0,axiom,
    ( c_uminus(c_uminus(V_y,tc_set(T_a)),tc_set(T_a)) = V_y )).

cnf(cls_Set_Oempty__iff_0,axiom,
    ( ~ c_in(V_c,c_emptyset,T_a) )).

cnf(cls_Set_Oempty__subsetI_0,axiom,
    ( c_lessequals(c_emptyset,V_A,tc_set(T_a)) )).

cnf(cls_Set_OsubsetI_0,axiom,
    ( c_in(c_Main_OsubsetI__1(V_A,V_B,T_a),V_A,T_a)
    | c_lessequals(V_A,V_B,tc_set(T_a)) )).

cnf(cls_Set_Osubset__antisym_0,axiom,
    ( ~ c_lessequals(V_B,V_A,tc_set(T_a))
    | ~ c_lessequals(V_A,V_B,tc_set(T_a))
    | V_A = V_B )).

cnf(cls_Set_Osubset__refl_0,axiom,
    ( c_lessequals(V_A,V_A,tc_set(T_a)) )).

%------------------------------------------------------------------------------
