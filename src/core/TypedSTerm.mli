
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Simple Typed Terms}.

These terms are scoped, and possibly typed. Type inference should be
performed on them. *)

type location = ParseLocation.t

type t
type term = t

type view = private
  | Var of string             (** variable *)
  | BVar of string            (** bound variable *)
  | Const of Symbol.t         (** constant *)
  | App of t * t list         (** apply term *)
  | Bind of Symbol.t * t * t  (** bind variable in term *)
  | Multiset of t list
  | Record of (string * t) list * t option  (** extensible record *)

val view : t -> view
val loc : t -> location option
val ty : t -> t option
val ty_exn : t -> t

include Interfaces.HASH with type t := t
include Interfaces.ORD with type t := t

(** {2 Constructors} *)

exception IllFormedTerm of string

val tType : t
val wildcard : t

val var : ?loc:location -> ?ty:t -> string -> t
val bvar : ?loc:location -> ?ty:t -> string -> t
val app : ?loc:location -> ?ty:t -> t -> t list -> t
val const : ?loc:location -> ?ty:t -> Symbol.t -> t
val bind : ?loc:location -> ?ty:t -> Symbol.t -> t -> t -> t
val bind_list : ?loc:location -> ?ty:t -> Symbol.t -> t list -> t -> t
val multiset : ?loc:location -> ?ty:t -> t list -> t
val record : ?loc:location -> ?ty:t -> (string*t) list -> rest:t option -> t
(** Build a record with possibly a row variable.
    @raise IllFormedTerm if the [rest] is not either a record or a variable. *)

val of_string : ?loc:location -> ?ty:t -> string -> t
val of_int : ?ty:t -> int -> t

val at_loc : ?loc:location -> t -> t
val with_ty : ?ty:t -> t -> t

val fresh_var : ?loc:location -> ?ty:t -> unit -> t
(** fresh free variable with the given type. *)

val fresh_bvar : ?loc:location -> ?ty:t -> unit -> t

(** {2 Utils} *)

val is_var : t -> bool
val is_bvar : t -> bool

val ground : t -> bool
(** [true] iff there is no free variable *)

val closed : t -> bool
(** [closed t] is [true] iff all bound variables of [t] occur under a
    binder (i.e. they are actually bound in [t]) *)

val vars : t -> t list

val close_all : ?ty:t -> Symbol.t -> t -> t
(** Bind all free vars with the symbol *)

include Interfaces.PRINT with type t := t

module Set : Sequence.Set.S with type elt = term
module Map : Sequence.Map.S with type key = term
module Tbl : Hashtbl.S with type key = term

module Seq : sig
  val subterms : t -> t Sequence.t
  val subterms_with_bound : t -> (t * Set.t) Sequence.t
  val vars : t -> t Sequence.t
end

(** {2 Visitor} *)

module Visitor : sig
  type 'a t = {
    var : term -> ?loc:location -> ?ty:'a -> string -> 'a;
    bvar : term -> ?loc:location -> ?ty:'a -> string -> 'a;
    app : term -> ?loc:location -> ?ty:'a -> 'a -> 'a list -> 'a;
    const : term -> ?loc:location -> ?ty:'a -> Symbol.t -> 'a;
    bind : term -> ?loc:location -> ?ty:'a -> Symbol.t -> 'a -> 'a -> 'a;
    multiset : term -> ?loc:location -> ?ty:'a -> 'a list -> 'a;
    record : term -> ?loc:location -> ?ty:'a ->
            (string*'a) list -> 'a option -> 'a;
  }
  (** Fold-like operation that maps a term into a value of type 'a *)

  val apply : visitor:'a t -> term -> 'a

  val id : term t
  val for_all : bool t
end

(** {2 Substitutions, Unification} *)

type 'a or_error = [`Error of string | `Ok of 'a]

module Subst : sig
  type t

  val empty : t

  val add : t -> term -> term -> t

  val eval : t -> term -> term

  val eval_head : t -> term -> term

  include Interfaces.PRINT with type t := t
end

val rename : term -> term
(** Rename all free variables *)

exception UnifyFailure of term * term * Subst.t

val unify : ?subst:Subst.t -> term -> term -> Subst.t or_error
(** Unify two terms, might fail *)

val unify_exn : ?subst:Subst.t -> term -> term -> Subst.t
(** Same as {!unify}, but can raise.
    @raise UnifyFailure if the unification fails *)
