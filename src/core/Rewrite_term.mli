
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Rewriting on Terms} *)

type term = FOTerm.t

(** {2 Types and Basics} *)

type rule

(** positions that are immediate arguments of some defined constant
    can be classified as follows:
    - active position (patterns on LHS of rules)
    - invariant positions (variable on LHS and RHS of rules)
    - accumulator positions (variable on LHS, non-variable on RHS)
*)
type defined_position =
  | Pos_active
  | Pos_invariant
  | Pos_accumulator

type defined_positions = defined_position IArray.t

type defined_cst
(** Payload of a defined function symbol *)

val pp_defined_position : defined_position CCFormat.printer
val pp_defined_positions : defined_positions CCFormat.printer

module Rule : sig
  type t = rule

  val lhs : t -> term
  val rhs : t -> term
  val vars : t -> FOTerm.VarSet.t
  val vars_l : t -> Type.t HVar.t list
  val head_id : t -> ID.t
  val args : t -> term list

  val make_const : ID.t -> Type.t -> term -> t
  (** [make_const id ty rhs] is the same as [T.const id ty --> rhs] *)

  val make : ID.t -> Type.t -> term list -> term -> t
  (** [make id ty args rhs] is the same as [T.app (T.const id ty) args --> rhs] *)

  include Interfaces.HASH with type t := t
  include Interfaces.ORD with type t := t
  include Interfaces.PRINT with type t := t
end

(** {2 Defined Cosntant}

    A constant that is defined by at least one term rewrite rule *)

module Defined_cst : sig
  type t = defined_cst
  val ty : t -> Type.t
  val rules_seq : t -> rule Sequence.t
  val defined_positions : t -> defined_positions
  val level : t -> int

  include Interfaces.PRINT with type t := t
end

(**/**)
exception Payload_defined_cst of defined_cst
(**/**)

val as_defined_cst : ID.t -> defined_cst option

val is_defined_cst : ID.t -> bool

val declare_defined_cst : ?level:int -> ID.t -> rule list -> defined_cst
(** [declare_defined_cst id rules] makes [id] a defined constant
    with the given (initial) set of rules
    @raise Invalid_argument if the ID is already a skolem or a constructor,
     or if the list of rules is empty
*)

val declare_cst_or_add : ID.t -> rule -> unit
(** [declare_cst_or_add id rule] defines [id] if it's not already a
    defined constant, and add [rule] to it *)

val add_rule : defined_cst -> rule -> unit
val add_rule_l : defined_cst -> rule list -> unit

(** {2 Rewriting and Narrowing} *)

module R_set : CCSet.S with type elt = rule

type rule_set = R_set.t

val pp_rule_set : rule_set CCFormat.printer

val normalize_term : term -> term * rule_set
(** [normalize t] computes the normal form of [t] w.r.t the set
    of rewrite rules stored in IDs.
    Returns the new term and the set of rules that were used *)

(* TODO: [app f l] which is the same as [T.app f l], but also reduces
   whenever [f] is a defined constant with one rule which matches [l] *)

val narrow_term :
  ?subst:Subst.t ->
  scope_rules:Scoped.scope ->
  term Scoped.t ->
  (rule * Subst.t) Sequence.t
(** [narrow_term ~scope_rule t] finds the set of rules [(l --> r)]
    in IDs and substitutions [sigma] such that [sigma(l) = sigma(t)] *)
