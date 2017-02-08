
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

module type S = Bool_lit_intf.S

type lit = Hornet_types.lit
type clause = Hornet_types.clause
type clause_idx = Hornet_types.clause_idx
type proof = Hornet_types.proof

type view = Bool_lit_intf.view =
  | Fresh of int
  | Box_clause of clause
  | Select_lit of clause * clause_idx
  | Ground_lit of lit (* must be ground *)

module Make(X : sig end) : S
