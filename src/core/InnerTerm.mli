
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Scoped Terms}

    Those terms are not designed to be used directly, but rather to provide
    a generic backend (implementing De Bruijn indices, subterms, substitutions,
    etc.) for other more specific representations like Type, Term, ...
*)

type t
(** Abstract type of term *)

type term = t

type view = private
  | Var of t HVar.t (** Free variable *)
  | DB of int
  | Bind of Binder.t * t * t (** Type, sub-term *)
  | Const of ID.t (** Constant *)
  | App of t * t list (** Uncurried application *)
  | AppBuiltin of Builtin.t * t list (** For representing special constructors *)

val view : t -> view
(** View on the term's head form *)

type type_result =
  | NoType
  | HasType of t

val ty : t -> type_result
(** Type of the term, if any *)

val ty_exn : t -> t
(** Same as {!ty}, but fails if the term has no type
    @raise Invalid_argument if the type is [NoType] *)

include Interfaces.HASH with type t := t
include Interfaces.ORD with type t := t

val hash_mod_alpha : t -> int
(** Hash invariant w.r.t variable renaming *)

val same_l : t list -> t list -> bool
(** Physical equality on lists of terms, roughly the same as {!List.forall2 (==)} *)

(** {3 Constructors}

    Some constructors, such as {!record}, may raise
    {!IllFormedTerm}if the arguments are ill-formed (several occurrences of
    a key), or, for variables, if the number is negative *)

exception IllFormedTerm of string
type nat = int

val const : ty:t -> ID.t -> t
val app : ty:t -> t -> t list -> t
val bind : ty:t -> varty:t -> Binder.t -> t -> t
val var : t HVar.t -> t
val bvar : ty:t -> nat -> t
val app_builtin : ty:t -> Builtin.t -> t list -> t
val builtin: ty:t -> Builtin.t -> t

val tType : t
(** The root of the type system. It doesn't have a type.
    It has kind [Kind.Type] *)

val arrow : t list -> t -> t
(** Smart constructor for arrow types *)

val cast : ty:t -> t -> t
(** Change the type *)

val is_var : t -> bool
val is_bvar : t -> bool
val is_const : t -> bool
val is_bind : t -> bool
val is_app : t -> bool

val hashcons_stats : unit -> int*int*int*int*int*int

(** {3 Payload} *)

exception No_payload

val payload : t -> exn

val set_payload : t -> exn -> unit
(** Set payload.
    @raise Invalid_argument if there is already a payload *)

val set_payload_erase : t -> exn -> unit
(** Set payload, ignoring the previous payload. *)

(** {3 Containers} *)

module Map : CCMap.S with type key = term
module Set : CCSet.S with type elt = term
module Tbl : CCHashtbl.S with type key = term

module VarMap : CCMap.S with type key = t HVar.t
module VarSet : CCSet.S with type elt = t HVar.t
module VarTbl : CCHashtbl.S with type key = t HVar.t

(** {3 De Bruijn indices handling} *)

module DB : sig
  type env = t DBEnv.t

  val closed : t -> bool
  (** check whether the term is closed (all DB vars are bound within the
      term). If this returns [true] then the term doesn't depend on
      its environment. *)

  val contains : t -> int -> bool
  (** Does t contains the De Bruijn variable of index n? *)

  val shift : int -> t -> t
  (** shift the non-captured De Bruijn indexes in the term by n *)

  val unshift : int -> t -> t
  (** [unshift n t] unshifts the term [t]'s bound variables by [n]. In
      other words it decrements indices of all free De Bruijn variables
      inside by [n]. Variables bound within [t] are left untouched. *)

  val replace : t -> sub:t -> t
  (** [db_from_term t ~sub] replaces [sub] by a fresh De Bruijn index in [t]. *)

  val from_var : t -> var:t -> t
  (** [db_from_var t ~var] replace [var] by a De Bruijn symbol in t.
      Same as {!replace}. *)

  val eval : env -> t -> t
  (** Evaluate the term in the given De Bruijn environment, by
      replacing De Bruijn indices by their value in the environment. *)

  val apply_subst : t VarMap.t -> t -> t
  (** Apply the given simple substitution to variables in [t]; if some
      variable [v] is bound to [t'], then [t'] can be open and will be
      shifted as required.
      Traverses the whole term. *)
end

(** {3 Iterators} *)

module Seq : sig
  val vars : t -> t HVar.t Sequence.t
  val subterms : t -> t Sequence.t
  val subterms_depth : t -> (t * int) Sequence.t  (* subterms with their depth *)
  val symbols : t -> ID.t Sequence.t
  val types : t -> t Sequence.t
  val max_var : t HVar.t Sequence.t -> int
  val min_var : t HVar.t Sequence.t -> int
  val add_set : Set.t -> t Sequence.t -> Set.t
  val add_tbl : unit Tbl.t -> t Sequence.t -> unit
end

(** {3 Positions} *)

module Pos : sig
  val at : t -> Position.t -> t
  (** retrieve subterm at pos, or raise Invalid_argument*)

  val replace : t -> Position.t -> by:t -> t
  (** replace t|_p by the second term *)
end

val replace : t -> old:t -> by:t -> t
(** [replace t ~old ~by] syntactically replaces all occurrences of [old]
    in [t] by the term [by]. *)

val replace_m : t -> t Map.t -> t
(** Simultaneous replacement of every [a->b] in the map *)

(** {3 Variables} *)

val bind_vars : ty:t -> Binder.t -> t HVar.t list -> t -> t
(** [bind_vars ~ty b vars t] binds each [v in vars] with the binder [b],
    with body [t], and each intermediate result has type [ty]
    (not suitable for functions) *)

val close_vars :  ty:t -> Binder.t -> t -> t
(** Close all free variables of the term using the binding symbol *)

val is_ground : t -> bool
(** [true] if the term contains no free variables *)

(** {3 Misc} *)

val size : t -> int

val depth : t -> int

val head : t -> ID.t option
(** Head symbol, or None if the term is a (bound) variable *)

val type_is_unifiable : t -> bool
(** Can we (syntactically) unify terms of this type? *)

(** {2 IO} *)

val print_hashconsing_ids : bool ref
(** if enabled, every term will be printed with its unique ID *)

val show_type_arguments : bool ref
(** Parameter for printing/hiding type arguments in terms *)

include Interfaces.PRINT with type t := t
include Interfaces.PRINT_DE_BRUIJN with type t := t
                                    and type term := t

val pp_var : t HVar.t CCFormat.printer

val add_default_hook : print_hook -> unit
(** Add a print hook that will be used from now on *)

val default_hooks: unit -> print_hook list

val debugf : t CCFormat.printer

val pp_zf : t CCFormat.printer

(* TODO: path-selection operation (for handling general-data in TPTP), see
        XSLT or CSS *)

(* TODO: functor for scoping operation (and inverse) between
        ScopedTerm and NamedTerm *)
