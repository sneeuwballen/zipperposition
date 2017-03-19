
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 First-order terms}

    Those terms provide a first-order presentation of higher-order terms (without
    functions), in the sense that they make currying possible (as well as applying
    functions to other terms).

    This is as if terms had an `apply` symbol everywhere, but more lightweight.

    Types and terms are mixed because it makes application much easier.
*)

(** {2 Term} *)

type t = private InnerTerm.t

type term = t

type var = Type.t HVar.t
(** Variables are typed with {!Type.t} *)

type view = private
  | AppBuiltin of Builtin.t * t list
  | DB of int (** Bound variable (De Bruijn index) *)
  | Var of var (** Term variable *)
  | Const of ID.t (** Typed constant *)
  | App of t * t list (** Application to a list of terms (cannot be left-nested) *)

val view : t -> view


(** {2 Classic view} *)
module Classic : sig
  type view = private
    | Var of var
    | DB of int
    | App of ID.t * t list (** covers Const and App *)
    | AppBuiltin of Builtin.t * t list
    | NonFO (** any other case *)

  val view : t -> view
end
(** {2 Comparison, equality, containers} *)

val subterm : sub:t -> t -> bool
(** checks whether [sub] is a (non-strict) subterm of [t] *)

include Interfaces.HASH with type t := t
include Interfaces.ORD with type t := t

val ty : t -> Type.t                (** Obtain the type of a term.. *)

module Set : CCSet.S with type elt = t
module Map : CCMap.S with type key = t
module Tbl : CCHashtbl.S with type key = t

val hash_mod_alpha : t -> int
(** Hash invariant w.r.t variable renaming *)

val same_l : t list -> t list -> bool
(** [same_l l1 l2] returns [true] if terms of [l1] and [l2] are pairwise
    equal, [false] otherwise.
    Precondition: both lists have the same length
    @raise Assert_failure if lists have not the same length *)

(** {2 Constructors} *)

val var : var -> t

val var_of_int : ty:Type.t -> int -> t

val bvar : ty:Type.t -> int -> t
(** Create a bound variable. Providing a type is mandatory.
    {b Warning}: be careful and try not to use this function directly.
    @raise InnerTerm.IllFormedTerm if the index is < 0 *)

val builtin : ty:Type.t -> Builtin.t -> t

val app_builtin : ty:Type.t -> Builtin.t -> t list -> t

val const : ty:Type.t -> ID.t -> t
(** Create a typed constant *)

val tyapp : t -> Type.t list -> t
(** Apply a term to types
    @raise Type.Error if types do not match. *)

val app : t -> t list -> t
(** Apply a term to a list of terms
    @raise Type.ApplyError if types do not match. *)

val app_full : t -> Type.t list -> t list -> t
(** Apply the term to types, then to terms *)

val true_ : t
val false_ : t

val grounding : Type.t -> t
(** [grounding ty] is a unique constant of type [ty] *)

val is_var : t -> bool
val is_bvar : t -> bool
val is_app : t -> bool
val is_const : t -> bool

val as_const : t -> ID.t option
val as_const_exn : t -> ID.t
val as_var : t -> var option
val as_var_exn : t -> var

val of_term_unsafe : InnerTerm.t -> t
(** {b NOTE}: this can break the invariants and make {!view} fail. Only
    apply with caution. *)

val of_ty : Type.t -> t
(** Upcast from type *)

module VarSet : CCSet.S with type elt = var
module VarMap : CCMap.S with type key = var
module VarTbl : CCHashtbl.S with type key = var

(** {2 Sequences} *)

module Seq : sig
  val vars : t -> var Sequence.t
  val subterms : t -> t Sequence.t
  val subterms_depth : t -> (t * int) Sequence.t  (* subterms with their depth *)
  val symbols : t -> ID.t Sequence.t
  val max_var : var Sequence.t -> int (** max var *)
  val min_var : var Sequence.t -> int (** min var *)
  val ty_vars : t -> var Sequence.t
  val typed_symbols : t -> (ID.t * Type.t) Sequence.t
  val add_set : Set.t -> t Sequence.t -> Set.t
end

val var_occurs : var:var -> t -> bool (** [var_occurs ~var t] true iff [var] in t *)
val is_ground : t -> bool (** is the term ground? (no free vars) *)
val monomorphic : t -> bool (** true if the term contains no type var *)
val max_var : VarSet.t -> int (** find the maximum variable *)
val min_var : VarSet.t -> int (** minimum variable *)
val add_vars : unit VarTbl.t -> t -> unit (** add variables of the term to the set *)
val vars : t -> VarSet.t (** compute variables of the terms *)
val vars_prefix_order : t -> var list (** variables in prefix traversal order *)
val depth : t -> int (** depth of the term *)
val head : t -> ID.t option (** head ID.t *)
val head_exn : t -> ID.t (** head ID.t (or Invalid_argument) *)
val size : t -> int (** Size (number of nodes) *)

val weight : ?var:int -> ?sym:(ID.t -> int) -> t -> int
(** Compute the weight of a term, given a weight for variables
    and one for ID.ts.
    @param var unique weight for every variable (default 1)
    @param sym function from ID.ts to their weight (default [const 1])
    @since 0.5.3 *)

val ty_vars : t -> Type.VarSet.t
(** Set of free type variables *)

(** {2 Subterms and Positions} *)

module Pos : sig
  val at : t -> Position.t -> t
  (** retrieve subterm at pos
      @raise Invalid_argument if the position is invalid *)

  val replace : t -> Position.t -> by:t -> t
  (** [replace t pos ~by] replaces the subterm at position [pos]
      in [t] by the term [by]. The two terms should have the same type.
      @raise Invalid_argument if the position is not valid *)
end

val replace : t -> old:t -> by:t -> t
(** [replace t ~old ~by] syntactically replaces all occurrences of [old]
    in [t] by the term [by]. *)

val replace_m : t -> t Map.t -> t
(** [replace t m] syntactically replaces all occurrences of bindings of
    the map in [t], starting from the root *)

(** {2 High-level operations} *)

val symbols : ?init:ID.Set.t -> t -> ID.Set.t
(** Symbols of the term (keys of signature) *)

val contains_symbol : ID.t -> t -> bool
(** Does the term contain this given ID.t? *)

(** {2 Fold} *)

(** High level fold-like combinators *)

val all_positions :
  ?vars:bool -> ?ty_args:bool -> ?pos:Position.t ->
  t -> t Position.With.t Sequence.t
(** Iterate on all sub-terms with their position.
    @param vars specifies whether variables are folded on (default false).
    @param ty_args specifies whether type arguments are folded on (default true).
    @param pos the initial position (default empty) *)

(** {2 Some AC-utils} *)

module type AC_SPEC = sig
  val is_ac : ID.t -> bool
  val is_comm : ID.t -> bool
end

module AC(A : AC_SPEC) : sig
  val flatten : ID.t -> t list -> t list
  (** [flatten_ac f l] flattens the list of terms [l] by deconstructing all its
      elements that have [f] as head ID.t. For instance, if l=[1+2; 3+(4+5)]
      with f="+", this will return [1;2;3;4;5], perhaps in a different order *)

  val normal_form : t -> t
  (** normal form of the term modulo AC *)

  val equal : t -> t -> bool
  (** Check whether the two terms are AC-equal. Optional arguments specify
      which ID.ts are AC or commutative (by default by looking at
      attr_ac and attr_commut). *)

  val symbols : t Sequence.t -> ID.Set.t
  (** Set of ID.ts occurring in the terms, that are AC *)

  val seq_symbols : t -> ID.t Sequence.t
  (** Sequence of AC symbols in this term *)
end

(** {2 Printing/parsing} *)

val print_all_types : bool ref
(** If true, {!pp} will print the types of all annotated terms *)

include Interfaces.PRINT with type t := t
include Interfaces.PRINT_DE_BRUIJN with type t := t
                                    and type term := t

val add_hook : print_hook -> unit
(** Hook used by default for printing *)

val default_hooks : unit -> print_hook list
(** List of default hooks *)

(* TODO
   include Interfaces.SERIALIZABLE with type t := t
*)

val debugf : Format.formatter -> t -> unit
(** debugf printing, with sorts *)

(** {2 Arith} *)

module Arith : sig
  val floor : t
  val ceiling : t
  val truncate : t
  val round : t

  val prec : t
  val succ : t

  val sum : t
  val difference : t
  val uminus : t
  val product : t
  val quotient : t

  val quotient_e : t
  val quotient_t : t
  val quotient_f : t
  val remainder_e : t
  val remainder_t : t
  val remainder_f : t

  val less : t
  val lesseq : t
  val greater : t
  val greatereq : t

  val pp_hook : print_hook
  (** hook to print arithmetic expressions *)
end

(** {2 TPTP} *)

module TPTP : sig
  include Interfaces.PRINT with type t := t
  include Interfaces.PRINT_DE_BRUIJN with type t := t
                                      and type term := t
                                      and type print_hook := print_hook
end

module Conv : sig
  type ctx
  val create : unit -> ctx
  val of_simple_term : ctx -> TypedSTerm.t -> t option
  val of_simple_term_exn : ctx -> TypedSTerm.t -> t (** @raise Type.Conv.Error on failure *)
  val to_simple_term :
    ?env:TypedSTerm.t Var.t DBEnv.t ->
    ctx ->
    t ->
    TypedSTerm.t
end
