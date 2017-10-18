
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Simple Typed Terms}. *)

(** Similar to {!STerm}, but this time the terms are properly
    scoped (using {!Var}) and typed.

    These terms are suitable for many preprocessing transformations,
    including {!CNF}.

    They can be obtained from {!STerm.t} using {!TypeInference}.
*)

type location = ParseLocation.t

type t
type term = t
type ty = t

(** a constructor of given type, applied to a list of type argumentss *)
type match_cstor = {
  cstor_id: ID.t;
  cstor_ty: ty;
  cstor_args: ty list;
}

type match_branch = match_cstor  * t Var.t list * t

type view = private
  | Var of t Var.t (** variable *)
  | Const of ID.t (** constant *)
  | App of t * t list (** apply term *)
  | Ite of t * t * t
  | Match of t * match_branch list
  | Let of (t Var.t * t) list * t
  | Bind of Binder.t * t Var.t * t (** bind variable in term *)
  | AppBuiltin of Builtin.t * t list
  | Multiset of t list
  | Record of (string * t) list * t option (** extensible record *)
  | Meta of meta_var (** Unification variable *)

(* a variable with a one-shot binding, and some annotation about
   whether it can be generalized *)
and meta_var = t Var.t * t option ref * [`Generalize | `BindDefault | `NoBind]

val view : t -> view
val loc : t -> location option
val ty : t -> t option
val ty_exn : t -> t
val head : t -> ID.t option
val head_exn : t -> ID.t (** @raise Not_found if not an application/const *)

val deref : t -> t
(** While [t] is a bound [Meta] variable, follow its link *)

include Interfaces.HASH with type t := t
include Interfaces.ORD with type t := t

(** {2 Constructors} *)

exception IllFormedTerm of string

val tType : t
val prop : t

val var : ?loc:location -> t Var.t -> t
val var_of_string : ?loc:location -> ty:t -> string -> t
val app : ?loc:location -> ty:t -> t -> t list -> t
val app_whnf : ?loc:location -> ty:t -> t -> t list -> t (** application + WHNF *)
val const : ?loc:location -> ty:t -> ID.t -> t
val const_of_cstor : ?loc:location -> match_cstor -> t
val ite : ?loc:location -> t -> t -> t -> t
val match_ : ?loc:location -> t -> match_branch list -> t
val let_ : ?loc:location -> (t Var.t * t) list -> t -> t
val app_builtin : ?loc:location -> ty:t -> Builtin.t -> t list -> t
val builtin : ?loc:location -> ty:t -> Builtin.t -> t
val bind : ?loc:location -> ty:t -> Binder.t -> t Var.t -> t -> t
val bind_list : ?loc:location -> ty:t -> Binder.t -> t Var.t list -> t -> t
val multiset : ?loc:location -> ty:t -> t list -> t
val meta : ?loc:location -> meta_var -> t
val record : ?loc:location -> ty:t -> (string*t) list -> rest:t Var.t option -> t
val record_flatten : ?loc:location -> ty:t -> (string*t) list -> rest:t option -> t
(** Build a record with possibly a row variable.
    @raise IllFormedTerm if the [rest] is not either a record or a variable. *)

val fun_l : ?loc:location -> t Var.t list -> t -> t

val of_string : ?loc:location -> ty:t -> string -> t
(** Make a constant from this string *)

val at_loc : ?loc:location -> t -> t

val with_ty : ty:t -> t -> t
val map_ty : t -> f:(t -> t) -> t

val fresh_var : ?loc:location -> ty:t -> unit -> t
(** fresh free variable with the given type. *)

val box_opaque : t -> t
(** Put a box around this *)

(** {2 Specific Views} *)

module Ty : sig
  type t = term

  type builtin = Prop | TType | Term | Int | Rat

  type view =
    | Ty_builtin of builtin
    | Ty_var of t Var.t
    | Ty_app of ID.t * t list
    | Ty_fun of t list * t
    | Ty_forall of t Var.t * t
    | Ty_multiset of t
    | Ty_record of (string * t) list * t Var.t option
    | Ty_meta of meta_var

  val view : t -> view

  include Interfaces.HASH with type t := t
  include Interfaces.ORD with type t := t

  val tType : t
  val var : ?loc:location -> t Var.t -> t
  val var_of_string : ?loc:location -> string -> t
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
  val real : t
  val term : t

  val (==>) : t list -> t -> t
  (** Alias to {!fun_} *)

  val close_forall : t -> t

  val unfold : t -> t Var.t list * t list * t
  (** [unfold [forall a b. x y z -> ret]] returns the triples
      [[a,b], [x,y,z], ret] *)

  val arity : t -> int * int
  (** [arity ty] returns [(n,m)] where [ty = forall x1..xn (a1 ... am -> ret)] *)

  val mangle : t -> string
  (** String usable as an identifier, without whitespace *)

  val needs_args : t -> bool
  (** [needs_args ty] means that [arity ty <> (0,0)] *)

  val is_tType : t -> bool
  val is_prop : t -> bool
  val returns : t -> t
  val returns_tType : t -> bool
  val returns_prop : t -> bool
end

val sort_ty_vars_first : t Var.t list -> t Var.t list
(** sort the given list of variables by putting type variables first *)

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
  val ite : ?loc:location -> t -> t -> t -> t
  val forall : ?loc:location -> t Var.t -> t -> t
  val exists : ?loc:location -> t Var.t -> t -> t

  val eq_or_equiv : t -> t -> t
  val neq_or_xor : t -> t -> t

  val forall_l : ?loc:location -> t Var.t list -> t -> t
  val exists_l : ?loc:location -> t Var.t list -> t -> t

  val unfold_binder : Binder.t -> t -> t Var.t list * t

  val unfold_forall : t -> t Var.t list * t
  val close_forall : ?loc:location -> t -> t

  val box_opaque : t -> t
end

(** {2 Utils} *)

val is_var : t -> bool
val is_meta : t -> bool
val is_const : t -> bool
val is_fun : t -> bool

val is_ground : t -> bool
(** [true] iff there is no free variable *)

val is_monomorphic : t -> bool
(** [true] if there are no type variables *)

val is_subterm : strict:bool -> t -> of_:t -> bool
(** [is_subterm a ~of_:b] is true if [a] is a subterm of [b].
    @param strict if true, [a] must be a strict subterm of [b],
      that is, not [b] itself *)

val closed : t -> bool
(** [closed t] is [true] iff all bound variables of [t] occur under a
    binder (i.e. they are actually bound in [t]) *)

val unfold_binder : Binder.t -> t -> t Var.t list * t
(** [unfold_binder b (b v1 (b v2... (b vn t)))] returns [[v1,...,vn], t] *)

val unfold_fun : t -> t Var.t list * t

val var_occurs : var:t Var.t -> t -> bool
(** [var_occurs ~var t] is [true] iff [var] occurs in [t] *)

val as_id_app : t -> (ID.t * Ty.t * t list) option

val vars : t -> t Var.t list
val free_vars : t -> t Var.t list
val free_vars_l : t list -> t Var.t list
val free_vars_set : t -> t Var.Set.t

val close_all : ty:t -> Binder.t -> t -> t
(** Bind all free vars with the symbol *)

(** Generic non-recursive map *)
val map :
  f:('a -> t -> t) ->
  bind:('a -> ty Var.t -> 'a * ty Var.t) ->
  'a ->
  t ->
  t

include Interfaces.PRINT with type t := t

val pp_inner : t CCFormat.printer
val pp_with_ty : t CCFormat.printer

val pp_in : Output_format.t -> t CCFormat.printer

module Set : Sequence.Set.S with type elt = term
module Map : Sequence.Map.S with type key = term
module Tbl : Hashtbl.S with type key = term

module Seq : sig
  val subterms : t -> t Sequence.t
  val subterms_with_bound : t -> (t * t Var.Set.t) Sequence.t
  val vars : t -> t Var.t Sequence.t
  val free_vars : t -> t Var.t Sequence.t
  val metas : t -> meta_var Sequence.t
end

(** {2 Substitutions} *)

module Subst : sig
  type t = (term, term) Var.Subst.t

  val empty : t

  val mem : t -> term Var.t -> bool

  val add : t -> term Var.t -> term -> t
  (** Add new binding to substitution
      Fails if the variable is bound already *)

  val find : t -> term Var.t -> term option

  val find_exn : t -> term Var.t -> term
  (** @raise Not_found if the variable is not present *)

  val rename_var : t -> term Var.t -> t * term Var.t

  val merge : t -> t -> t

  val eval : t -> term -> term

  val eval_nonrec : t -> term -> term
  (** Evaluate under substitution, but consider the substitution as
      not idempotent *)

  include Interfaces.PRINT with type t := t
end

val rename : (term, term Var.t) Var.Subst.t -> t -> t
(** Perform renaming *)

val rename_all_vars : t -> t
(** Rename bound variables *)

(** {2 Table of Variables} *)

module Var_tbl : CCHashtbl.S with type key = t Var.t

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
  ?gen_fresh_meta:(unit -> meta_var) ->
  ?allow_open:bool -> ?loc:location -> ?st:UStack.t -> ?subst:Subst.t ->
  t -> t list -> t
(** [apply_unify f_ty args] compute the type of a function of type [f_ty],
    when applied to parameters [args]. The first elements of [args] might
    be interpreted as types, the other ones as terms (whose types are unified
    against expected types). *)

val app_infer :
  ?st:UStack.t -> ?subst:Subst.t ->
  t -> t list -> t
(** [app_infer f l] computes the type [ty] of [f l], and return [app ~ty f l]
    @raise UnifyFailure if types do not correspond *)

(** {2 Conversion} *)

val erase : t -> STerm.t

(** {2 TPTP} *)

module TPTP : sig
  include Interfaces.PRINT with type t := t
end

module ZF : sig
  include Interfaces.PRINT with type t := t
  val pp_inner : t CCFormat.printer
end

