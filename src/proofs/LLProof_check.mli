
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Check LLProof} *)

(** TODO: the checker itself.

    GOAL: A tiny Tableau-like prover that tries to re-prove each step in a
    simple way.
*)

open Logtk

type proof = LLProof.t

type res =
  | R_ok
  | R_fail

val pp_res : res CCFormat.printer

type stats = {
  n_ok: int; (** steps that were successfully checked *)
  n_fail: int; (** steps that failed *)
  n_skip: int; (** steps skipped, not checked *)
}

val pp_stats : stats CCFormat.printer

val check :
  ?before_check:(proof -> unit) ->
  ?on_check:(proof -> res option -> unit) ->
  proof ->
  res * stats

