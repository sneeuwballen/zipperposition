
(* This file is free software, part of Libzipperposition. See file "license" for more details. *)

(** {1 Simple Typed Terms}.

These terms are scoped, and possibly typed. Type inference should be
performed on them. *)

type location = ParseLocation.t

type t
type term = t

type view = private
  | Var of t Var.t (** variable *)
  | Const of ID.t (** constant *)
  | App of t * t list (** apply term *)
  | Bind of Binder.t * t Var.t * t (** bind variable in term *)
  | AppBuiltin of Builtin.t * t list
  | Multiset of t list
  | Record of (string * t) list * t option (** extensible record *)
  | Meta of meta_var (** Unification variable *)

(* a variable with a one-shot binding *)
and meta_var = t Var.t * t option ref

val view : t -> view
val loc : t -> location option
val ty : t -> t option
val ty_exn : t -> t

val deref : t -> t
(** While [t] is a bound [Meta] variable, follow its link *)

include Interfaces.HASH with type t := t
include Interfaces.ORD with type t := t

(** {2 Constructors} *)

exception IllFormedTerm of string

val tType : t
val prop : t

val var : ?loc:location -> t Var.t -> t
val app : ?loc:location -> ty:t -> t -> t list -> t
val const : ?loc:location -> ty:t -> ID.t -> t
val app_builtin : ?loc:location -> ty:t -> Builtin.t -> t list -> t
val builtin : ?loc:location -> ty:t -> Builtin.t -> t
val bind : ?loc:location -> ty:t -> Binder.t -> t Var.t -> t -> t
val bind_list : ?loc:location -> ty:t -> Binder.t -> t Var.t list -> t -> t
val multiset : ?loc:location -> ty:t -> t list -> t
val meta : ?loc:location -> meta_var -> t
val meta_of_string : ?loc:location -> ty:t -> string -> t (** Fresh meta *)
val record : ?loc:location -> ty:t -> (string*t) list -> rest:t Var.t option -> t
val record_flatten : ?loc:location -> ty:t -> (string*t) list -> rest:t option -> t
(** Build a record with possibly a row variable.
    @raise IllFormedTerm if the [rest] is not either a record or a variable. *)

val of_string : ?loc:location -> ty:t -> string -> t
(** Make a constant from this string *)

val at_loc : ?loc:location -> t -> t

val with_ty : ty:t -> t -> t
val map_ty : t -> f:(t -> t) -> t

val fresh_var : ?loc:location -> ty:t -> unit -> t
(** fresh free variable with the given type. *)

(** {2 Specific Views} *)

module Ty : sig
  type t = term

  type builtin = Prop | TType | Term | Int | Rat

  type view =
    | Builtin of builtin
    | Var of t Var.t
    | App of ID.t * t list
    | Fun of t list * t
    | Forall of t Var.t * t
    | Multiset of t
    | Record of (string * t) list * t Var.t option
    | Meta of meta_var

  val view : t -> view

  val tType : t
  val var : ?loc:location -> t Var.t -> t
  val meta : ?loc:location -> meta_var -> t
  val fun_ : ?loc:location -> t list -> t -> t
  val app : ?loc:location -> ID.t -> t list -> t
  val const : ?loc:location -> ID.t -> t
  val forall : ?loc:location -> t Var.t -> t -> t
  val forall_l : ?loc:location -> t Var.t list -> t -> t
  val multiset : ?loc:location -> t -> t
  val record : ?loc:location -> (string * t) list -> rest:t Var.t option -> t
  val record_flatten : ?loc:location -> (string * t) list -> rest:t option -> t

  val prop : t
  val int : t
  val rat : t
  val term : t

  val (==>) : t list -> t -> t
  (** Alias to {!fun_} *)

  val close_forall : t -> t

  val arity : t -> int * int
  (** [arity ty] returns [(n,m)] where [ty = forall x1..xn (a1 ... am -> ret)] *)

  val is_tType : t -> bool
  val returns : t -> t
  val returns_tType : t -> bool
end

module Form : sig
  type t = term
  type view =
    | True
    | False
    | Atom of t
    | Eq of t * t
    | Neq of t * t
    | Equiv of t * t
    | Xor of t * t
    | Imply of t * t
    | And of t list
    | Or of t list
    | Not of t
    | Forall of t Var.t * t
    | Exists of t Var.t * t

  val view : t -> view

  (** Smart constructors (perform simplifications) *)

  val true_ : t
  val false_ : t
  val atom : t -> t
  val eq : ?loc:location -> t -> t -> t
  val neq : ?loc:location -> t -> t -> t
  val equiv : ?loc:location -> t -> t -> t
  val xor : ?loc:location -> t -> t -> t
  val imply : ?loc:location -> t -> t -> t
  val and_ : ?loc:location -> t list -> t
  val or_ : ?loc:location -> t list -> t
  val not_ : ?loc:location -> t -> t
  val forall : ?loc:location -> t Var.t -> t -> t
  val exists : ?loc:location -> t Var.t -> t -> t

  val forall_l : ?loc:location -> t Var.t list -> t -> t
  val exists_l : ?loc:location -> t Var.t list -> t -> t

  val close_forall : ?loc:location -> t -> t
end

(** {2 Utils} *)

val is_var : t -> bool
val is_meta : t -> bool

val is_ground : t -> bool
(** [true] iff there is no free variable *)

val is_monomorphic : t -> bool
(** [true] if there are no type variables *)

val closed : t -> bool
(** [closed t] is [true] iff all bound variables of [t] occur under a
    binder (i.e. they are actually bound in [t]) *)

val var_occurs : var:t Var.t -> t -> bool
(** [var_occurs ~var t] is [true] iff [var] occurs in [t] *)

val vars : t -> t Var.t list
val free_vars : t -> t Var.t list

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
  val free_vars : t -> t Var.t Sequence.t
  val metas : t -> (t Var.t * t option ref) Sequence.t
end

(** {2 Substitutions} *)

module Subst : sig
  type t = (term, term) Var.Subst.t

  val empty : t

  val mem : t -> term Var.t -> bool

  val add : t -> term Var.t -> term -> t

  val find : t -> term Var.t -> term option

  val find_exn : t -> term Var.t -> term
  (** @raise Not_found if the variable is not present *)

  val eval : t -> term -> term

  val eval_head : t -> term -> term

  include Interfaces.PRINT with type t := t
end

(** {2 Unification} *)

exception UnifyFailure of string * (term * term) list * location option

val pp_stack : (term * term) list CCFormat.printer

module UStack : sig
  type t
  (** Unification stack, for backtracking purposes *)

  val create : unit -> t

  type snapshot
  (** A snapshot of bindings at a given moment *)

  val snapshot : st:t -> snapshot
  (** Save current state *)

  val restore : st:t -> snapshot -> unit
  (** Restore all references to their state at [snapshot]. Bindings
      done since are undone. *)
end

val unify :
  ?allow_open:bool -> ?loc:location -> ?st:UStack.t -> ?subst:Subst.t ->
  term -> term -> unit
(** unifies destructively the two given terms, by modifying references
      that occur under {!Meta}. Regular variables are not modified.
    @param allow_open if true, metas can be unified to terms
      with free variables (default false)
    @param st used for backtracking
    @param subst substitution for bound variables
    @raise UnifyFailure if unification fails. *)

val apply_unify :
  ?allow_open:bool -> ?loc:location -> ?st:UStack.t -> ?subst:Subst.t ->
  t -> t list -> t
(** [apply_unify f_ty args] compute the type of a function of type [f_ty],
    when applied to parameters [args]. The first elements of [args] might
    be interpreted as types, the other ones as terms (whose types are unified
    against expected types). *)

(** {2 Conversion} *)

val erase : t -> STerm.t

(** {2 TPTP} *)

module TPTP : sig
  include Interfaces.PRINT with type t := t
end

