
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Simple Terms}. *)

(** Simple terms, that are not hashconsed, nor typed.

    Those do not use De Bruijn indices for variable binding,
    but simply names (scoping is done later).
    Their simplicity make them good for heavy AST transformations, output of
    parsing, etc.

    Terms are only compared, hashsed, etc. by their "term" component (the algebraic
    variant). Additional fields (location…) are ignored for almost every
    operation.
*)

type location = ParseLocation.t

type var =
  | V of string
  | Wildcard

type t = private {
  term : view;
  loc : location option;
}

and match_branch =
  | Match_case of string * var list * t
  | Match_default of t

and view =
  | Var of var (** variable *)
  | Const of string (** constant *)
  | AppBuiltin of Builtin.t * t list
  | App of t * t list (** apply term *)
  | Ite of t * t * t
  | Match of t * match_branch list
  | Let of (var * t) list * t
  | Bind of Binder.t * typed_var list * t (** bind n variables *)
  | List of t list (** special constructor for lists *)
  | Record of (string * t) list * var option (** extensible record *)

and typed_var = var * t option

type term = t

val view : t -> view
val loc : t -> location option

include Interfaces.HASH with type t := t
include Interfaces.ORD with type t := t

val var : ?loc:location -> string -> t
val v_wild : t (** wildcard *)
val mk_var : ?loc:location -> var -> t
val app : ?loc:location -> t -> t list -> t
val app_const : ?loc:location -> string -> t list -> t
val builtin : ?loc:location -> Builtin.t -> t
val app_builtin : ?loc:location -> Builtin.t -> t list -> t
val const : ?loc:location -> string -> t
val bind : ?loc:location -> Binder.t -> typed_var list -> t -> t
val ite : ?loc:location -> t -> t -> t -> t
val match_ : ?loc:location -> t -> match_branch list -> t
val let_ : ?loc:location -> (var * t) list -> t -> t
val list_ : ?loc:location -> t list -> t
val nil : t
val record : ?loc:location -> (string*t) list -> rest:var option -> t
val at_loc : loc:location -> t -> t

val wildcard : t

val is_app : t -> bool
val is_var : t -> bool

val true_ : t
val false_ : t

val and_ : ?loc:location -> t list -> t
val or_ : ?loc:location -> t list -> t
val not_ : ?loc:location -> t -> t
val equiv : ?loc:location -> t -> t -> t
val xor : ?loc:location -> t -> t -> t
val imply : ?loc:location -> t -> t -> t
val eq : ?loc:location -> t -> t -> t
val neq : ?loc:location -> t -> t -> t
val forall : ?loc:location -> typed_var list -> t -> t
val exists : ?loc:location -> typed_var list -> t -> t
val lambda : ?loc:location -> typed_var list -> t -> t
val int_ : Z.t -> t
val of_int : int -> t
val rat : Q.t -> t
val real : string -> t

val tType : t
val term : t
val prop : t
val ty_int : t
val ty_rat : t
val ty_real : t
val fun_ty : ?loc:location -> t list -> t -> t
val forall_ty : ?loc:location -> typed_var list -> t -> t

val ty_unfold : t -> t list * t
val unfold_bind: Binder.t -> t -> typed_var list * t

module Set : CCSet.S with type elt = term
module Map : CCMap.S with type key = term
module Tbl : CCHashtbl.S with type key = term

module StringSet : CCSet.S with type elt = string

module Seq : sig
  val vars : t -> var Sequence.t
  val free_vars : t -> string Sequence.t
  val subterms : t -> t Sequence.t
  val subterms_with_bound : t -> (t * StringSet.t) Sequence.t
  (** subterm and variables bound at this subterm *)

  val symbols : t -> string Sequence.t
end

val ground : t -> bool
val close_all : Binder.t -> t -> t  (** Bind all free vars with the symbol *)
val subterm : strict:bool -> t -> sub:t -> bool
(** is [sub] a (strict?) subterm of the other arg? *)

(** {2 Print} *)

include Interfaces.PRINT with type t := t
val pp_typed_var : typed_var CCFormat.printer
val pp_var : var CCFormat.printer

(** {2 Formats} *)

module TPTP : sig
  include Interfaces.PRINT with type t := t
end

module ZF : sig
  include Interfaces.PRINT with type t := t
  val pp_inner : t CCFormat.printer
end

val pp_in : Output_format.t -> t CCFormat.printer

(** {2 Subst} *)

module StrMap : CCMap.S with type key = string

type subst = t StrMap.t

val empty_subst : subst

val merge_subst : subst -> subst -> subst
(** [merge a b] merges [a] into [b], but favors [b] in case
    of conflict *)

val apply_subst : subst -> term -> term
