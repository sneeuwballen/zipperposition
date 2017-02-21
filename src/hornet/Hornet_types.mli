
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Common Type Declarations} *)

(** We gather here the main data structures, because they are
    often mutually recursive *)

open Libzipperposition

type ty = Type.t
type term = FOTerm.t
type var = ty HVar.t
type formula = TypedSTerm.t
type bool_unique_id = int

type 'a var_map = 'a Type.VarMap.t

type clause = {
  c_id: int; (* unique ID *)
  c_lits: lit IArray.t;
  c_kind: c_kind; (* mut: can update the horn clause *)
  c_proof: proof;
  c_trail: bool_trail; (* components/splits the clause depends on *)
  c_depth: int; (* number of instantiations in its proof *)
  mutable c_constr: c_constraint;
  mutable c_select: select_lit option; (* if there is currently a selected lit *)
  mutable c_grounding: bool_lit IArray.t option; (* grounding *)
}

and lit =
  | Bool of bool
  | Atom of term * bool
  | Eq of term * term * bool

(* internal kind *)
and c_kind =
  | C_horn of horn_clause lazy_t
  | C_general

and proof =
  | P_trivial
  | P_from_input of Statement.role (* added to input *)
  | P_from_file of Statement.from_file * Statement.role
  | P_cnf_neg of proof_with_res
  | P_cnf of proof_with_res
  | P_instance of clause * Subst.t
  | P_avatar_split of clause
  (* given clause has been split into var-disjoint components,
     one of which is the current clause *)
  | P_avatar_cut of horn_clause * (bool_lit * proof) list (* cut the given literals *)
  | P_split of clause * select_lit * c_constraint (* model-driven recursive splitting *)
  | P_bool_tauto (* boolean tautology *)
  | P_bool_res of bool_res_step
  | P_bool_grounding of clause (* grounding of clause *)
  | P_hc_superposition of hc_superposition_step
  | P_hc_eq_res of horn_clause * Subst.t (* equality resolution *)
  | P_hc_simplify of horn_clause
  | P_hc_demod of horn_clause * horn_clause list (* passive, active set *)

and proof_with_res = proof * proof_res

and proof_res =
  | PR_formula of formula
  | PR_horn_clause of horn_clause
  | PR_clause of clause
  | PR_bool_clause of bool_clause

and c_constraint = {
  constr_dismatch: Dismatching_constr.t list;
}

and horn_clause = {
  hc_id: int; (* unique ID *)
  hc_head: lit;
  hc_body: lit IArray.t;
  hc_constr: c_constraint;
  hc_trail: bool_trail;
  hc_proof: proof;
  hc_unordered_depth: int; (* how many unordered inferences needed? *)
  hc_label: label;
  mutable hc_status: (horn_clause_status * int);
  (* where is the clause in its lifecycle? int=number of cycles *)
}

and horn_clause_status =
  | HC_alive (** Alive and kicking *)
  | HC_dead (** Unregistered, inert *)

(* clause + substitution, for grounding purpose *)
and labelled_clause = {
  lc_clause: clause; (* invariant: non horn *)
  lc_sel: select_lit; (* the selected lit in [lc_clause] *)
  lc_subst: term var_map; (* substitution to instantiate the clause *)
  lc_real_subst: Subst.t lazy_t;
}

(* label of a Horn clause: a set of labelled clauses *)
and label = labelled_clause list

(** Description of a single superposition step *)
and hc_superposition_step = {
  hc_sup_active: horn_clause Scoped.t; (* positive unit *)
  hc_sup_passive: horn_clause Scoped.t; (* non-unit *)
  hc_sup_active_pos: Position.t;
  hc_sup_passive_pos: Position.t;
  hc_sup_s: term; (* LHS of active eqn *)
  hc_sup_t: term; (* RHS of active eqn *)
  hc_sup_rewritten: term; (* unifies with [s] *)
  hc_sup_subst: Subst.t;
}

(** Description of a single boolean resolution step
    between two clauses *)
and bool_res_step = {
  bool_res_c1: bool_clause;
  bool_res_p1: proof;
  bool_res_c2: bool_clause;
  bool_res_p2: proof;
  bool_res_atom: bool_lit;
}

and bool_atom =
  | A_fresh of bool_unique_id
  | A_box_clause of bool_box_clause
  | A_ground of bool_ground

and bool_box_clause = {
  bool_box_id: bool_unique_id;
  bool_box_clause: clause;
  mutable bool_box_depends : horn_clause list; (* clauses depending on this *)
}

(* selection of a literal in a non-ground non-horn clause *)
and select_lit = {
  select_idx: clause_idx;
  select_lit: lit; (* [lit = get clause idx] *)
  mutable select_depends : horn_clause list; (* clauses depending on this *)
}

and bool_ground = {
  bool_ground_lit: lit;
  bool_ground_id: int;
  mutable bool_ground_instance_of: (clause*clause_idx) list;
  (* clauses whose instance contain this ground lit (at given index) *)
}

(* index of a literal in a clause *)
and clause_idx = int

and bool_lit = {
  bl_atom: bool_atom;
  bl_sign: bool;
}

and bool_clause = bool_lit list

and bool_trail = bool_lit lazy_t list
(** A boolean trail, guarding the clauses that hold only in some models *)

(* stages in the solver's algorithm *)
type stage =
  | Stage_init
  | Stage_presaturate
  | Stage_start
  | Stage_exit

type event =
  | E_add_component of bool_box_clause
  | E_remove_component of bool_box_clause
  | E_select_lit of clause * select_lit * c_constraint
  (** [lit | constr] has been selected in some clause *)
  | E_unselect_lit of clause * select_lit
  | E_add_ground_lit of bool_ground
  | E_remove_ground_lit of bool_ground
  | E_if_sat (** final check of the model *)
  | E_conflict of bool_trail * label * proof (* boolean conflict in some theory *)
  | E_found_unsat of proof_with_res (* final proof *)
  | E_stage of stage
