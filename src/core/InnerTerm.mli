
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Inner Terms} *)

(** Those terms are not designed to be used directly, but rather to provide
    a generic backend (implementing De Bruijn indices, subterms, substitutions,
    etc.) for other more specific representations like {!Type.t} and {!Term.t}.

    The point is that we only have to do substitution, hashconsing,
    and De Bruijn indices manipulation in one place;
    it also makes it easy to make terms
    and types occur in one another (types as parameters to terms, etc.)

    NOTE: this should be used with a lot of caution. Few checks are performed
    (even typing checks) and it is easy to obtain non-closed terms
    or ill-typed terms by manipulating this carelessly.
*)

type t = private {
  term : view;
  ty : type_result;
  mutable id : int;
  mutable payload: exn;
}
(** Abstract type of term *)

and view = private
  | Var of t HVar.t (** Free variable *)
  | DB of int
  | Bind of Binder.t * t * t (** Type, sub-term *)
  | Const of ID.t (** Constant *)
  | App of t * t list (** Uncurried application *)
  | AppBuiltin of Builtin.t * t list (** For representing special constructors *)

and type_result =
  | NoType
  | HasType of t

type term = t

val view : t -> view
(** View on the term's head form *)

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
val is_tType : t -> bool

val is_lambda : t -> bool

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

  val shift : ?depth:int -> int -> t -> t
  (** shift the non-captured De Bruijn indexes in the term by n *)

  val unshift : ?depth:int -> int -> t -> t
  (** [unshift n t] unshifts the term [t]'s bound variables by [n]. In
      other words it decrements indices of all free De Bruijn variables
      inside by [n]. Variables bound within [t] are left untouched. *)

  val replace : t -> sub:t -> t
  (** [replace t ~sub] replaces [sub] by a fresh De Bruijn index in [t].
      Shifts other De Bruijn indices by 1 *)

  val replace_l : t -> l:t list -> t
  (** N-ary version of {!replace}
      Shifts other De Bruijn indices by [length t] *)

  val from_var : t -> var:t -> t
  (** [db_from_var t ~var] replace [var] by a De Bruijn symbol in t.
      Same as {!replace}. *)

  val eval : env -> t -> t
  (** Evaluate the term in the given De Bruijn environment, by
      replacing De Bruijn indices by their value in the environment. *)
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

val fun_: t -> t -> t

val fun_l : t list -> t -> t

val fun_of_fvars : t HVar.t list -> t -> t
(** Build a function from a list of free vars + the body.
    This performs the De Bruijn transformation, and shifts the body. *)

val open_fun : t -> t list * t
(** [open_fun ty] "unrolls" function arrows from the left, so that
    [open_fun (a -> (b -> (c -> d)))] returns [[a;b;c], d].
    @return the return type and the list of all its arguments *)

val open_poly_fun : t -> int * t list * t
(** [open_poly_fun ty] "unrolls" polymorphic function arrows from the left, so that
    [open_fun (forall a b. f a -> (g b -> (c -> d)))] returns [2; [f a;g b;c], d].
    @return the return type, the number of type variables,
      and the list of all its arguments *)

val expected_ty_vars : t -> int
(** @return the number of type variables that a type requires. *)

val open_bind : Binder.t -> t -> t list * t

val open_bind_fresh : Binder.t -> t -> t HVar.t list * t
(** [open_bind_fresh λ (λxy. F)] returns [[v1,v2], F[v1/x,v2/y]]
    where [v1] and [v2] are fresh variables using {!HVar.fresh} *)

val open_bind_fresh2 :
  ?eq_ty:(t -> t -> bool) ->
  Binder.t -> t -> t ->
  t HVar.t list * t * t
(** [open_bind_free2 λ (λxy. F) (λxyz. G)]
    returns [[v1,v2], F[v1/x,v2/y], λz.G[v1/x,v2/y]]
    where [v1] and [v2] are fresh variables using {!HVar.fresh}
    @param eq_ty checks whether type of bound variables are compatible *)

val open_fun : t -> t list * t
(** [open_fun ty] "unrolls" function arrows from the left, so that
    [open_fun (a -> (b -> (c -> d)))] returns [[a;b;c], d].
    @return the return type and the list of all its arguments *)

val open_poly_fun : t -> int * t list * t
(** [open_poly_fun ty] "unrolls" polymorphic function arrows from the left, so that
    [open_fun (forall a b. f a -> (g b -> (c -> d)))] returns [2; [f a;g b;c], d].
    @return the return type, the number of type variables,
      and the list of all its arguments *)

val open_bind : Binder.t -> t -> t list * t

val open_bind2 : Binder.t -> t -> t -> t list * t * t list * t

val mk_fun : ty_l:t list -> t -> t
(** [mk_fun ~ty_l body] closes over body for DB indices of type [ty_l] *)

val is_ground : t -> bool
(** [true] if the term contains no free variables *)

(** {3 Misc} *)

val size : t -> int

val depth : t -> int

val head : t -> ID.t option
(** Head symbol, or None if the term is a (bound) variable *)

val type_is_unifiable : t -> bool
(** Can we (syntactically) unify terms of this type? *)

val type_non_unifiable_tags: t -> Builtin.Tag.t list
(** Theory tags that justify this type not being unifiable *)

val type_is_prop : t -> bool
(** Is is equal to [prop] *)

val is_a_type : t -> bool
(** Is this a type? (i.e. its type is {!tType}) *)

val as_app : t -> t * t list
(** [as_app t] decomposes [t] into a head (non-application) and arguments,
    such as [(let f,l = as_app t in app f l) = t] *)

val as_var : t -> t HVar.t option
val as_var_exn : t -> t HVar.t

val as_bvar_exn : t -> int
val is_bvar_i : int -> t -> bool
(** [is_bvar_i n t] is [true] iff [t = bvar i] *)

(** {2 IO} *)

val print_hashconsing_ids : bool ref
(** if enabled, every term will be printed with its unique ID *)

val print_all_types : bool ref
(** if enabled, print all types *)

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

val pp_in : Output_format.t -> t CCFormat.printer

(* TODO: path-selection operation (for handling general-data in TPTP), see
        XSLT or CSS *)

(* TODO: functor for scoping operation (and inverse) between
        ScopedTerm and NamedTerm *)
