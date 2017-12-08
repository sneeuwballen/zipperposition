
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Types} *)

(** {2 Main Type representation}

    Types are represented using InnerTerm, with kind Type. Therefore, they
    are hashconsed and scoped.

    Common representation of types, including higher-order
    and polymorphic types. All type variables
    are assumed to be universally quantified in the outermost possible
    scope (outside any other quantifier).

    See {!TypeInference} for inferring types from terms and formulas,
    and {!Signature} to associate types with symbols.

    TODO: think of a good way of representating AC operators (+, ...)
*)

type t = private InnerTerm.t
(** Type is a subtype of the term structure
    (itself a subtype of InnerTerm.t),
    with explicit conversion *)

type ty = t

type builtin = TType | Prop | Term | Rat | Int

val pp_builtin : builtin CCFormat.printer
val builtin_conv : builtin -> Builtin.t

type view = private
  | Builtin of builtin
  | Var of t HVar.t
  | DB of int
  | App of ID.t * t list (** parametrized type *)
  | Fun of t list * t (** Function type (left to right, no left-nesting) *)
  | Forall of t (** explicit quantification using De Bruijn index *)

val view : t -> view
(** Type-centric view of the head of this type.
    @raise Assert_failure if the argument is not a type *)

include Interfaces.HASH with type t := t
include Interfaces.ORD with type t := t

val is_tType : t -> bool
val is_var : t -> bool
val is_bvar : t -> bool
val is_app : t -> bool
val is_const : t -> bool
val is_fun : t -> bool
val is_forall : t -> bool
val is_prop : t -> bool

val hash_mod_alpha : t -> int
(** Hash invariant w.r.t variable renaming *)

(** {2 Constructors} *)

val tType : t
val prop : t
val term : t
val int : t
val rat : t

val var : t HVar.t -> t

val var_of_int : int -> t
(** Build a type variable. *)

val app : ID.t -> t list -> t
(** Parametrized type *)

val builtin : builtin -> t

val const : ID.t -> t
(** Constant sort *)

val arrow : t list -> t -> t
(** [arrow l r] is the type [l -> r]. *)

val forall : t -> t
(** Quantify over one type variable. Careful with the De Bruijn indices! *)

val forall_n : int -> t -> t
(** Quantify over [n] type variable. Careful with the De Bruijn indices! *)

val forall_fvars : t HVar.t list -> t -> t
(** [forall_fvars vars body] makes the De Bruijn conversion before quantifying
    on [vars] *)

val bvar : int -> t
(** bound variable *)

val (==>) : t list -> t -> t
(** General function type. [l ==> x] is the same as [x] if [l]
    is empty. Invariant: the return type is never a function type. *)

val of_term_unsafe : InnerTerm.t -> t
(** {b NOTE}: this can break the invariants and make {!view} fail. Only
    use with caution. *)

val of_terms_unsafe : InnerTerm.t list -> t list
val cast_var_unsafe : InnerTerm.t HVar.t -> t HVar.t

(** {2 Definition} *)

type def =
  | Def_unin of int (* number of type variables *)
  | Def_data of int * ty list (* data type with number of variables and cstors *)

val def : ID.t -> def option
(** Access the definition of a type *)

val def_exn : ID.t -> def
(** Unsafe version of {!def}
    @raise Not_found if not a proper constant *)

val set_def : ID.t -> def -> unit
(** Set definition of an ID *)

(** {2 Containers} *)

module Set : CCSet.S with type elt = t
module Map : CCMap.S with type key = t
module Tbl : CCHashtbl.S with type key = t

module Seq : sig
  val vars : t -> t HVar.t Sequence.t
  val sub : t -> t Sequence.t (** Subterms *)
  val symbols : t -> ID.t Sequence.t
  val add_set : Set.t -> t Sequence.t -> Set.t
  val max_var : t HVar.t Sequence.t -> int
  val min_var : t HVar.t Sequence.t -> int
end

(** {2 Utils} *)

module VarSet : CCSet.S with type elt = t HVar.t
module VarMap : CCMap.S with type key = t HVar.t
module VarTbl : CCHashtbl.S with type key = t HVar.t

val vars_set : VarSet.t -> t -> VarSet.t
(** Add the free variables to the given set *)

val vars : t -> t HVar.t list
(** List of free variables *)

val close_forall : t -> t
(** bind free variables *)

type arity_result =
  | Arity of int * int
  | NoArity

val arity : t -> arity_result
(** Number of arguments the type expects.
    If [arity ty] returns [Arity (a, b)] that means that it
    expects [a] arguments to be used as arguments of Forall, and
    [b] arguments to be used for function application. If
    it returns [NoArity] then the arity is unknown (variable) *)

val expected_args : t -> t list
(** Types expected as function argument by [ty]. The length of the
    list [expected_args ty] is the same as [snd (arity ty)]. *)

val expected_ty_vars : t -> int
(** Number of type parameters expected. 0 for monomorphic types. *)

val needs_args : t -> bool
(** [needs_args ty] iff [expected_ty_vars ty>0 || expected_args ty<>[]] *)

val order : t -> int
(** Number of left-nested function types (1 for constant and variables).
    [order (a->b) = 1]
    [order ((a->b)->c) = 2]
    [order (((a->b)->c)->d) = 2] *)

val is_ground : t -> bool
(** Is the type ground? (means that no {!Var} not {!BVar} occurs in it) *)

val size : t -> int
(** Size of type, in number of "nodes" *)

val depth : t -> int
(** Depth of the type (length of the longest path to some leaf)
    @since 0.5.3 *)

val open_poly_fun : t -> int * t list * t
(** [open_poly_fun ty] "unrolls" polymorphic function arrows from the left, so that
    [open_fun (forall a b. f a -> (g b -> (c -> d)))] returns [2; [f a;g b;c], d].
    @return the return type, the number of type variables,
      and the list of all its arguments *)

val open_fun : t -> t list * t
(** [open_fun ty] "unrolls" function arrows from the left, so that
    [open_fun (a -> (b -> (c -> d)))] returns [[a;b;c], d].
    @return the return type and the list of all its arguments *)

val returns : t -> t
(** returned type (going through foralls and arrows).
    [returns a] is like [let _, _, ret = open_poly_fun a in ret]
    {b NOTE} caution, not always closed *)

val returns_prop : t -> bool
val returns_tType : t -> bool

exception ApplyError of string
(** Error raised when {!apply} fails *)

val apply : t -> t list -> t
(** Given a function/forall type, and arguments, return the
    type that results from applying the function/forall to the arguments.
    No unification is done, types must check exactly.
    @raise ApplyError if the types do not match *)

val apply1 : t -> t -> t
(** [apply1 a b] is short for [apply a [b]]. *)

val apply_unsafe : t -> InnerTerm.t list -> t
(** Similar to {!apply}, but assumes its arguments are well-formed
    types without more ado.
    @raise ApplyError if types do not match
    @raise Assert_failure if the arguments are not proper types *)

val is_unifiable : t -> bool
(** Are terms of this type syntactically unifiable?
    See {!InnerTerm.type_is_unifiable} *)

(** {2 IO} *)

include Interfaces.PRINT_DE_BRUIJN with type term := t and type t := t
include Interfaces.PRINT with type t := t
val pp_surrounded : t CCFormat.printer
val pp_typed_var : t HVar.t CCFormat.printer

val mangle : t -> string
val pp_mangle : t CCFormat.printer

(** {2 TPTP} specific printer and types *)

module TPTP : sig
  include Interfaces.PRINT_DE_BRUIJN with type term := t and type t := t
  include Interfaces.PRINT with type t := t
  val pp_typed_var : t HVar.t CCFormat.printer

  (** {2 Basic types} *)

  val i : t       (** individuals *)
  val o : t       (** propositions *)

  val int : t     (** integers *)
  val rat : t     (** rationals *)
  val real : t    (** reals *)
end

module ZF : sig
  include Interfaces.PRINT with type t := t
  val pp_typed_var : t HVar.t CCFormat.printer
end

val pp_in : Output_format.t -> t CCFormat.printer

(** {2 Conversions} *)

module Conv : sig
  type ctx
  val create : unit -> ctx
  val copy : ctx -> ctx
  val clear : ctx -> unit

  val of_simple_term : ctx -> TypedSTerm.t -> t option
  (** convert a simple typed term into a type. The term is assumed to be
        closed.
      @return an error message if the term is not a type
      @param ctx context used to map {!Var} to {!HVar} *)

  val var_of_simple_term : ctx -> TypedSTerm.t Var.t -> t HVar.t
  (** Convert a variable (and its type), and remember the binding. *)

  val fresh_ty_var : ctx -> t HVar.t
  (** Fresh type variable *)

  val var_to_simple_var : ?prefix:string -> ctx -> t HVar.t -> TypedSTerm.t Var.t

  exception Error of TypedSTerm.t

  val of_simple_term_exn : ctx -> TypedSTerm.t -> t
  (** @raise Invalid_argument if conversion is impossible *)

  val to_simple_term :
    ?env:TypedSTerm.t Var.t DBEnv.t ->
    ctx ->
    t ->
    TypedSTerm.t
    (** convert a type to a prolog term.
        @param env the current environement for De Bruijn indices *)
end


(**/**)
val rebuild_rec : ?env:t list -> t -> t (** rebuild recursively and checks *)
val unsafe_eval_db : t list -> t -> t
(**/**)
