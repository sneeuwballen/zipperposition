
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Constraint for a Clause} *)

open Hornet_types

type t = c_constraint

let empty : t = {
  constr_dismatch=Dismatching_constr.empty;
}

let pp = Hornet_types_util.pp_constraint
let to_string = CCFormat.to_string pp

let is_trivial (c:t): bool =
  Dismatching_constr.is_trivial c.constr_dismatch

let is_absurd (c:t): bool =
  Dismatching_constr.is_absurd c.constr_dismatch

let add_dismatch (d:Dismatching_constr.t) (c:t): t =
  { constr_dismatch = Dismatching_constr.combine c.constr_dismatch d; }

let apply_subst ~renaming subst (c,sc): t =
  { constr_dismatch =
      Dismatching_constr.apply_subst ~renaming subst (c.constr_dismatch,sc);
  }

let combine (a:t)(b:t): t =
  { constr_dismatch =
      Dismatching_constr.combine a.constr_dismatch b.constr_dismatch;
  }
