
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Simple Clause} *)

open Logtk

type flag

type t = private {
  id : int; (** unique ID of the clause *)
  lits : Literal.t array; (** the literals *)
  trail : Trail.t; (** boolean trail *)
  mutable flags : flag; (** boolean flags for the clause *)
}

(** {2 Basics} *)

val make : trail:Trail.t -> Literal.t array -> t

val equal : t -> t -> bool
val compare : t -> t -> int
val hash : t -> int
val id : t -> int
val lits : t -> Literal.t array
val trail : t -> Trail.t

val is_empty : t -> bool

val length : t -> int

val update_trail : (Trail.t -> Trail.t) -> t -> t

val to_s_form :
  ?allow_free_db:bool -> ?ctx:Term.Conv.ctx ->
  t -> TypedSTerm.Form.t

(** {2 Flags} *)

val flag_lemma : flag (** clause is a lemma *)
val flag_persistent : flag (** clause cannot be redundant *)
val flag_redundant : flag (** clause has been shown to be redundant *)
val flag_backward_simplified : flag (** clause has been backward simplified *)

val set_flag : flag -> t -> bool -> unit (** set boolean flag *)
val get_flag : flag -> t -> bool (** get value of boolean flag *)
val new_flag : unit -> flag (** new flag that can be used on clauses *)

val mark_redundant : t -> unit
val is_redundant : t -> bool
val mark_backward_simplified : t -> unit
val is_backward_simplified : t -> bool

(** {2 IO} *)

val pp_vars : t CCFormat.printer

val pp : t CCFormat.printer
val pp_zf : t CCFormat.printer
val pp_tstp : t CCFormat.printer
val pp_tstp_full : t CCFormat.printer  (** Print in a toplevel TPTP statement *)

val pp_trail : Trail.t CCFormat.printer
val pp_trail_tstp : Trail.t CCFormat.printer

val pp_in : Output_format.t -> t CCFormat.printer

(** {2 Proofs} *)

val proof_tc : t Proof.Result.tc

val mk_proof_res : t -> Proof.Result.t

val adapt : Proof.S.t -> t -> Proof.S.t
