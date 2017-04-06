
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Check LLProof} *)

(** A tiny Tableau-like prover that tries to re-prove each step in a
    simple way. *)

type proof = LLProof.t

type res =
  | R_ok
  | R_fail

val pp_res : res CCFormat.printer

type stats = {
  n_ok: int;
  n_fail: int;
  n_nocheck: int;
}

val pp_stats : stats CCFormat.printer

val check :
  ?before_check:(proof -> unit) ->
  ?on_check:(proof -> res option -> unit) ->
  proof ->
  res * stats

