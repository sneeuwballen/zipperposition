
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Common Type Declarations} *)

(** We gather here the main data structures, because they are
    often mutually recursive *)

open Libzipperposition

type ty = Type.t
type term = FOTerm.t
type bool_unique_id = int

type clause = {
  c_id: int; (* unique ID *)
  c_lits: lit IArray.t;
  c_kind: c_kind;
  c_proof: proof;
  c_trail: bool_trail; (* components/splits the clause depends on *)
  mutable c_constr: c_constraint_ list;
}

and lit =
  | Bool of bool
  | Atom of term * bool
  | Eq of term * term * bool

(* internal kind *)
and c_kind =
  | C_horn of horn_clause
  | C_general

and proof =
  | P_from_stmt of Statement.clause_t
  | P_instance of clause * Subst.t
  | P_avatar_split of clause
  (* given clause has been split into var-disjoint components,
     one of which is the current clause *)
  | P_split of clause (* model-driven recursive splitting *) (* TODO *)
  | P_bool_tauto (* boolean tautology *)
  | P_bool_res of bool_res_step
  | P_hc_superposition of hc_superposition_step
  | P_hc_simplify of horn_clause

and c_constraint_ =
  | C_dismatch of Dismatching_constr.t

and horn_clause = {
  hc_id: int; (* unique ID *)
  hc_head: lit;
  hc_body: lit IArray.t;
  hc_constr: c_constraint_ list;
  hc_trail: bool_trail;
  hc_proof: proof;
  hc_unordered_depth: int; (* how many unordered inferences needed? *)
  mutable hc_status: horn_clause_status; (* where is the clause in its lifecycle? *)
}

and horn_clause_status =
  | HC_new (** Just created, not registered *)
  | HC_alive (** Alive and kicking *)
  | HC_dead (** Unregistered, inert *)

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
}

and bool_atom =
  | A_fresh of bool_unique_id
  | A_box_clause of bool_box_clause
  | A_select of bool_select
  | A_ground of bool_ground

and bool_box_clause = {
  bool_box_id: bool_unique_id;
  bool_box_clause: clause;
  mutable bool_box_depends : horn_clause list; (* clauses depending on this *)
}

and bool_select = {
  bool_select_clause: clause;
  bool_select_idx: clause_idx;
  bool_select_lit: lit; (* [lit = get clause idx] *)
  bool_select_id: bool_unique_id;
  mutable bool_select_depends : horn_clause list; (* clauses depending on this *)
}

and bool_ground = {
  bool_ground_lit: lit;
  bool_ground_id: int;
  mutable bool_ground_instance_of: clause list;
  (* clauses whose instance contain this ground lit *)
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
  | E_select_lit of bool_select * Dismatching_constr.t list
  (** [lit | constr] has been selected in some clause *)
  | E_unselect_lit of bool_select
  | E_add_ground_lit of bool_ground
  | E_remove_ground_lit of bool_ground
  | E_conflict of bool_clause * proof (* boolean conflict in some theory *)
  | E_found_unsat of proof (* final proof *)
  | E_stage of stage
