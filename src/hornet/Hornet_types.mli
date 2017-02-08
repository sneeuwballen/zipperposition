
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Common Type Declarations} *)

(** We gather here the main data structures, because they are
    often mutually recursive *)

open Libzipperposition

type ty = Type.t
type term = FOTerm.t
type bool_unique_id = int

type clause = {
  c_lits: lit IArray.t;
  c_kind: c_kind;
  c_proof: proof;
  c_constr: c_constraint_ list;
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
  | P_avatar_split of clause (* split into var-disjoint components *)
  | P_split of clause (* model-driven recursive splitting *)
  | P_superposition of hc_superposition_step

and c_constraint_ =
  | C_dismatch of Dismatching_constr.t

and horn_clause = {
  hc_head: lit;
  hc_body: lit IArray.t;
  hc_constr: c_constraint_ list;
  hc_proof: proof;
}

(** Description of a single superposition step *)
and hc_superposition_step = {
  hc_sup_active: horn_clause; (* positive unit *)
  hc_sup_passive: horn_clause; (* non-unit *)
  hc_sup_active_pos: Position.t;
  hc_sup_passive_pos: Position.t;
  hc_sup_subst: Subst.t;
}

(* TODO: for "ground", make it point to a mutable list of clauses whose
   grounding contain this literal. Makes for efficient incremental selection.
*)
type bool_atom =
  | A_fresh of bool_unique_id
  | A_box_clause of clause * bool_unique_id
  | A_select of clause * clause_idx * bool_unique_id
  | A_ground of lit

(* index of a literal in a clause *)
and clause_idx = int

type bool_lit = {
  bl_atom: bool_atom;
  bl_sign: bool;
}
