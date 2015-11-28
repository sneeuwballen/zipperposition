
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Simple Typed Terms}.

These terms are scoped, and possibly typed. Type inference should be
performed on them. *)

type location = ParseLocation.t

type t
type term = t

type view = private
  | Var of t Var.t            (** variable *)
  | Const of Symbol.t         (** constant *)
  | App of t * t list         (** apply term *)
  | Bind of Binder.t * t Var.t * t  (** bind variable in term *)
  | AppBuiltin of Builtin.t * t list
  | Multiset of t list
  | Record of (string * t) list * t option  (** extensible record *)
  | Meta of t Var.t * t option ref (** Unification variable *)

val view : t -> view
val loc : t -> location option
val ty : t -> t option
val ty_exn : t -> t

val deref : t -> t

include Interfaces.HASH with type t := t
include Interfaces.ORD with type t := t

(** {2 Constructors} *)

exception IllFormedTerm of string

val tType : t
val prop : t

val var : ?loc:location -> t Var.t -> t
val app : ?loc:location -> ty:t -> t -> t list -> t
val const : ?loc:location -> ty:t -> Symbol.t -> t
val app_builtin : ?loc:location -> ty:t -> Builtin.t -> t list -> t
val builtin : ?loc:location -> ty:t -> Builtin.t -> t
val bind : ?loc:location -> ty:t -> Binder.t -> t Var.t -> t -> t
val bind_list : ?loc:location -> ty:t -> Binder.t -> t Var.t list -> t -> t
val multiset : ?loc:location -> ty:t -> t list -> t
val meta : ?loc:location -> t Var.t -> t
val meta_of_string : ?loc:location -> ty:t -> string -> t
val meta_full : ?loc:location -> t Var.t -> t option ref -> t
val record : ?loc:location -> ty:t -> (string*t) list -> rest:t option -> t
(** Build a record with possibly a row variable.
    @raise IllFormedTerm if the [rest] is not either a record or a variable. *)

val of_string : ?loc:location -> ty:t -> string -> t
(** Make a constant from this string *)

val at_loc : ?loc:location -> t -> t

val with_ty : ty:t -> t -> t
val map_ty : t -> f:(t -> t) -> t

val fresh_var : ?loc:location -> ty:t -> unit -> t
(** fresh free variable with the given type. *)

(** {2 Utils} *)

val is_var : t -> bool
val is_bvar : t -> bool

val ground : t -> bool
(** [true] iff there is no free variable *)

val closed : t -> bool
(** [closed t] is [true] iff all bound variables of [t] occur under a
    binder (i.e. they are actually bound in [t]) *)

val vars : t -> t list

val close_all : ty:t -> Binder.t -> t -> t
(** Bind all free vars with the symbol *)

include Interfaces.PRINT with type t := t

module Set : Sequence.Set.S with type elt = term
module Map : Sequence.Map.S with type key = term
module Tbl : Hashtbl.S with type key = term

module Seq : sig
  val subterms : t -> t Sequence.t
  val subterms_with_bound : t -> (t * t Var.Set.t) Sequence.t
  val vars : t -> t Var.t Sequence.t
  val metas : t -> (t Var.t * t option ref) Sequence.t
end

(** {2 Substitutions} *)


module Subst : sig
  type t

  val empty : t

  val add : t -> ID.t -> term -> t

  val find : t -> ID.t -> t option

  val find_exn : t -> ID.t -> t
  (** @raise Not_found if the variable is not present *)

  val eval : t -> term -> term

  val eval_head : t -> term -> term

  include Interfaces.PRINT with type t := t
end

exception TypeApplyError of t * t * string

val ty_apply : t -> t list -> t
(** Apply a (polymorphic) type to a list of arguments
    @raise TypeApplyError if some mismatch occurs *)

(** {2 Unification} *)

exception UnifyFailure of string * (term * term) list

module UStack : sig
  type t
  (** Unification stack, for backtracking purposes *)

  val create : unit -> t

  type snapshot
  (** A snapshot of bindings at a given moment *)

  val snapshot : st:t -> snapshot

  val restore : st:t -> snapshot -> unit
end

val unify : ?st:UStack.t-> term -> term -> unit
(** unifies destructively the two given terms, by modifying references
      that occur under {!Meta}. Regular variables are not modified.
    @param unif_stack used for backtracking
    @raise UnifyFailure if unification fails. *)
