(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Inference and simplification rules for the superposition calculus} *)

open Logtk
open Libzipperposition

(** {2 Inference rules} *)

val section : Util.Section.t

val k_unif_alg :
  (Term.t Scoped.t -> Term.t Scoped.t -> Unif_subst.t option OSeq.t)
  Flex_state.key

val k_ho_basic_rules : bool Flex_state.key

type unif_module = {
  unify_scoped:
    Term.t Scoped.t -> Term.t Scoped.t -> Unif_subst.t option OSeq.t;
  unify_scoped_l:
    Term.t list Scoped.t -> Term.t list Scoped.t -> Unif_subst.t option OSeq.t;
}

val get_unif_module : Env.t -> unif_module

val register : unit -> unit
(** Register the superposition module to its Environment's mixtbl. Done
    automatically by the {!extension}. *)

(** {2 As Extension}
    Extension named "superposition" *)

val extension : Extensions.t
