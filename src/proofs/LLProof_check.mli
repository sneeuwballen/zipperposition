
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Check LLProof} *)

(** TODO: the checker itself.

    GOAL: A tiny Tableau-like prover that tries to re-prove each step in a
    simple way.
*)

type proof = LLProof.t

type res =
  | R_ok
  | R_fail

val pp_res : res CCFormat.printer

type stats = {
  n_ok: int; (** steps that were successfully checked *)
  n_fail: int; (** steps that failed *)
  n_skip_esa: int; (** steps skipped because ESA *)
  n_skip_tags: int; (** steps skipped because of theory tags *)
  n_skip_trivial: int; (** steps skipped because they are trivial *)
  n_skip: int; (** steps skipped, not checked *)
}

val pp_stats : stats CCFormat.printer

(** Result for checking only one step *)
type check_step_res =
  | CS_check of res
  | CS_skip of [`ESA | `Other | `Tags | `Trivial]

val check :
  ?dot_prefix:string ->
  ?before_check:(proof -> unit) ->
  ?on_check:(proof -> check_step_res -> unit) ->
  proof ->
  res * stats

