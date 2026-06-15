(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Clauses} *)

open Logtk

type proof_step = Proof.Step.t
type proof = Proof.t

val stat_clause_create : Util.stat

type t
(** Abstract type for a clause *)

type sets = {
  c_set: t CCVector.ro_vector;  (** main set of clauses *)
  c_sos: t CCVector.ro_vector;  (** set of support *)
}
(** Bundle of clause sets *)

(** {2 Boolean flags} *)

type flag = SClause.flag

val set_flag : flag -> t -> bool -> unit
val get_flag : flag -> t -> bool
val mark_redundant : t -> unit
val is_redundant : t -> bool
val mark_backward_simplified : t -> unit
val is_backward_simplified : t -> bool
val is_orphaned : t -> bool

(** {2 Basics} *)

include Interfaces.EQ with type t := t
include Interfaces.HASH with type t := t

val compare : t -> t -> int
val id : t -> int
val lits : t -> Literal.t array
val is_ground : t -> bool
val weight : t -> int
val ho_weight : t -> int

module Tbl : CCHashtbl.S with type key = t

val is_goal : t -> bool
val distance_to_goal : t -> int option
val comes_from_goal : t -> bool

(** {2 Boolean Abstraction} *)

val pp_trail : Trail.t CCFormat.printer
val has_trail : t -> bool
val trail : t -> Trail.t
val trail_l : t list -> Trail.t
val update_trail : (Trail.t -> Trail.t) -> t -> t
val trail_subsumes : t -> t -> bool
val is_active : t -> v:Trail.valuation -> bool
val is_inj_axiom : t -> (Name.t * int) option

(** {2 Constructors} *)

val create :
  ctx:Ctx.t -> penalty:int -> trail:Trail.t -> Literal.t list -> proof_step -> t

val create_a :
  ctx:Ctx.t ->
  penalty:int ->
  trail:Trail.t ->
  Literal.t array ->
  proof_step ->
  t

val of_sclause : ctx:Ctx.t -> ?penalty:int -> SClause.t -> proof_step -> t

val of_forms :
  ctx:Ctx.t ->
  ?penalty:int ->
  trail:Trail.t ->
  Term.t SLiteral.t list ->
  proof_step ->
  t

val of_forms_axiom :
  ctx:Ctx.t ->
  ?penalty:int ->
  file:string ->
  name:string ->
  Term.t SLiteral.t list ->
  t

val of_statement :
  ctx:Ctx.t -> ?convert_defs:bool -> Statement.clause_t -> t list

(** {2 Proof} *)

val proof_step : t -> proof_step
val proof : t -> proof
val proof_parent : t -> Proof.Parent.t

val proof_parent_subst :
  Subst.Renaming.t -> t Scoped.t -> Subst.t -> Proof.Parent.t

val update_proof : t -> (proof_step -> proof_step) -> t
val proof_depth : t -> int

(** {2 Literal selection and eligibility} *)

val is_empty : t -> bool
val length : t -> int
val maxlits : ctx:Ctx.t -> t Scoped.t -> Subst.t -> CCBV.t
val is_maxlit : ctx:Ctx.t -> t Scoped.t -> Subst.t -> idx:int -> bool
val eligible_res : ctx:Ctx.t -> t Scoped.t -> Subst.t -> CCBV.t
val eligible_res_no_subst : ctx:Ctx.t -> t -> CCBV.t
val eligible_param : ctx:Ctx.t -> t Scoped.t -> Subst.t -> CCBV.t
val is_eligible_param : ctx:Ctx.t -> t Scoped.t -> Subst.t -> idx:int -> bool
val eligible_subterms_of_bool : ctx:Ctx.t -> t -> SClause.TPSet.t
val has_selected_lits : t -> bool
val is_selected : t -> int -> bool
val selected_lits : t -> (Literal.t * int) list
val selected_lits_bv : t -> CCBV.t
val bool_selected : t -> (Term.t * Logtk.Position.t) list
val penalty : t -> int
val inc_penalty : t -> int -> unit

(** {2 Properties} *)

val is_unit_clause : t -> bool
val is_oriented_rule : t -> bool
val symbols : ?init:Name.Set.t -> ?include_types:bool -> t Iter.t -> Name.Set.t

(** {2 Conversion} *)

val to_sclause : t -> SClause.t
val to_forms : t -> Term.t SLiteral.t list
val to_s_form : t -> TypedSTerm.Form.t
val ground_clause : t -> t
val eta_reduce : t -> t option

(** {2 Sets} *)

module ClauseSet : module type of struct
  include Set.Make (struct
    type nonrec t = t

    let compare = compare
  end)
end

val pp : t CCFormat.printer
val pp_tstp : t CCFormat.printer
val pp_tstp_full : t CCFormat.printer
val pp_set : ClauseSet.t CCFormat.printer
val pp_set_tstp : ClauseSet.t CCFormat.printer
val pp_tstp_list : t list CCFormat.printer

module WithPos : sig
  type elt = t
  type t

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val pp : t CCFormat.printer
  val term : t -> Term.t
  val clause : t -> t
  val pos : t -> Logtk.Position.t
  val lits : t -> Literal.t array
  val literals : t -> Literal.t
  val is_pos : t -> bool
end

module Seq : sig
  val lits : t -> Literal.t Iter.t
  val forms : t -> Term.t SLiteral.t Iter.t
  val terms : t -> Term.t Iter.t
  val vars : t -> Term.var Iter.t
  val symbols : t -> Name.t Iter.t
end

(** {2 Debug helpers} *)

val check_types : t -> unit
