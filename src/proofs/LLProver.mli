
(** {1 Low Level Prover} *)

(** A small theorem prover that checks entailment of ground formulas,
    with higher order terms and some theories *)

type form = LLTerm.Form.t

type res =
  | R_ok
  | R_fail

type final_state

val can_check : LLProof.tag list -> bool
(** Is this set of tags accepted by the tableau prover? *)

val prove : form list -> form -> res * final_state
(** [prove a b] returns [R_ok] if [a => b] is a tautology. *)

val pp_stats : final_state CCFormat.printer

val pp_dot : final_state CCFormat.printer

