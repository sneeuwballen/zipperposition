(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Terms} *)

(** Those terms provide a direct presentation of higher-order terms with lambdas
    in the sense that they make currying possible (as well as applying
    functions to other terms).

    This is as if terms had an `apply` symbol everywhere, but more lightweight.

    Types and terms are mixed because it makes application much easier
    (applying to a type and to a term are the same thing).
    It might also make dependent typing possible some day.
*)

(** {2 Term} *)

type t = private InnerTerm.t
type term = t

type var = Type.t HVar.t
(** Variables are typed with {!Type.t} *)

type view = private
   | AppBuiltin of Builtin.t * t list
   | DB of int  (** Bound variable (De Bruijn index) *)
   | Var of var  (** Term variable *)
   | Const of ID.t  (** Typed constant *)
   | App of t * t list  (** Application to a list of terms (cannot be left-nested) *)
   | Fun of Type.t * t  (** Lambda abstraction *)

val view : t -> view

(** {2 Classic view}

    This module provides a first-order view on terms, for code
    that focuses on first-order logic. *)
module Classic : sig
  type view = private
     | Var of var
     | DB of int
     | App of ID.t * t list  (** covers Const and App *)
     | AppBuiltin of Builtin.t * t list
     | NonFO  (** any other case *)

  val view : t -> view
end
(** {2 Comparison, equality, containers} *)

val subterm : sub:t -> t -> bool
(** checks whether [sub] is a (non-strict) subterm of [t] *)

include Interfaces.HASH with type t := t
include Interfaces.ORD with type t := t

val ty : t -> Type.t
(** Obtain the type of a term.. *)

module IntMap : Map.S with type key = int
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

val same_l_gen : t list -> t list -> bool
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

val app_w_ty : ty:Type.t -> t -> t list -> t
(** Apply a term to a list of terms
    @raise Type.ApplyError if types do not match. *)

val app_full : t -> Type.t list -> t list -> t
(** Apply the term to types, then to terms *)

val true_ : t
val false_ : t
val fun_ : Type.t -> t -> t
val fun_l : Type.t list -> t -> t

val fun_of_fvars : var list -> t -> t
(** Build a function from a list of free vars + the body.
    This performs the De Bruijn transformation, and shifts the body. *)

val open_fun : t -> Type.t list * t

val open_fun_offset : offset:int -> t -> var list * t * int
(** [open_fun ~offset (λxy. F)] returns [[v1,v2], F[v1/x,v2/y], offset+2]
    where [v1] and [v2] are fresh variables starting from offset *)

val grounding : Type.t -> t
(** [grounding ty] is a unique constant of type [ty] *)

val is_var : t -> bool
val is_appbuiltin : t -> bool
val is_bvar : t -> bool
val is_formula : t -> bool
val is_app : t -> bool
val is_const : t -> bool
val is_fun : t -> bool
val is_app_var : t -> bool

val is_type : t -> bool
(** Does it have type [tType]? *)

val in_pfho_fragment : t -> bool
val in_lfho_fragment : t -> bool
val is_fo_term : t -> bool
val in_fool_fragment : t -> bool * bool
val is_true_or_false : t -> bool

val lambda_depth : t -> int option
(** If term has no lambdas reutrn None;
   otherwise, return Some d where d is the
   maximal level of lambda nestings *)

val comb_depth : t -> int option
(** combinatory equivalent to lambda_depth *)

val hd_is_comb : Builtin.t -> bool
val is_comb : t -> bool
val mk_fresh_skolem : ?prefix:string -> var list -> Type.t -> (ID.t * Type.t) * t
val as_const : t -> ID.t option
val as_const_exn : t -> ID.t
val as_var : t -> var option
val as_var_exn : t -> var
val as_bvar_exn : t -> int

val as_app : t -> t * t list
(** [as_app t] decomposes [t] into a head (non-application) and arguments,
    such as [(let f,l = as_app t in app f l) = t] *)

val as_app_mono : t -> t * t list
(** [as_app_mono t] decomposes [t] into a head (possibly applied to type arguments)
    and arguments, such as [(let f,l = as_app_mono t in app f l) = t] *)

val as_fun : t -> Type.t list * t
(** Open functions *)

val head_term : t -> t
(** [head_term t = fst (as_app t)] *)

val head_term_mono : t -> t
(** head term, but still with type arguments *)

val as_app_mono : t -> t * t list
(** head term, but still with type arguments and the remaining arguments *)

val args : t -> t list
(** [args t = snd (as_app t)] *)

val ty_args: t -> Type.t list
(** Type arguments of the term *)

val of_term_unsafe : InnerTerm.t -> t
(** {b NOTE}: this can break the invariants and make {!view} fail. Only
    apply with caution. *)

val of_term_unsafe_l : InnerTerm.t list -> t list

val of_ty : Type.t -> t
(** Upcast from type *)

val mk_tmp_cst : counter:int ref -> ty:Type.t -> t
val is_properly_encoded : t -> bool

module VarSet : CCSet.S with type elt = var
module VarMap : CCMap.S with type key = var
module VarTbl : CCHashtbl.S with type key = var

(** {2 Iters} *)

module Seq : sig
  val vars : t -> var Iter.t
  val subterms : ?include_builtin:bool -> ?include_app_vars:bool -> ?ignore_head:bool -> t -> t Iter.t
  val subterms_depth : ?filter_term:(t -> bool) -> t -> (t * int) Iter.t (* subterms with their depth *)
  val symbols : ?include_types:bool -> ?filter_term:(t -> bool) -> t -> ID.t Iter.t

  val max_var : var Iter.t -> int
  (** max var *)

  val min_var : var Iter.t -> int
  (** min var *)

  val ty_vars : t -> var Iter.t
  val typed_symbols : ?include_types:bool -> t -> (ID.t * Type.t) Iter.t
  val add_set : Set.t -> t Iter.t -> Set.t

  (* given terms s and t, iterate over all terms s' t'
     such that s = u[s'] and t = u[t'] and u is non-empty context *)
  val common_contexts : t -> t -> (t * t) Iter.t
end

val var_occurs : var:var -> t -> bool
(** [var_occurs ~var t] true iff [var] in t *)

val is_ground : t -> bool
(** is the term ground? (no free vars) *)

(** is the term ground? (no free vars) *)
val is_linear : t -> bool
(** is the term linear? (no vars occurring multiple times) *)

(** is the term linear? (no vars occurring multiple times) *)
val monomorphic : t -> bool
(** true if the term contains no type var *)

val is_beta_reducible : t -> bool
val has_lambda : t -> bool

val max_var : VarSet.t -> int
(** find the maximum variable *)

val min_var : VarSet.t -> int
(** minimum variable *)

val add_vars : unit VarTbl.t -> t -> unit
(** add variables of the term to the set *)

val vars : t -> VarSet.t
(** compute variables of the terms *)

val vars_prefix_order : t -> var list
(** variables in prefix traversal order *)

val depth : t -> int
(** depth of the term *)

val head : t -> ID.t option
(** head ID.t *)

val head_exn : t -> ID.t
(** head ID.t (or Invalid_argument) *)

val size : t -> int
(** Size (number of nodes) *)

(* Sort the arguments to logical operators using their weights
   in an attempt to make more terms unifiable. *)
val normalize_bools : t -> t

(* all the ways in which term can be covered (built) using the arguments given *)
val cover_with_terms : ?depth:int -> ?recurse:bool -> t -> t option list -> t list

(* cover the term in a maximal way looked top-down *)
val max_cover : t -> t option list -> t

val weight : ?var:int -> ?sym:(ID.t -> int) -> t -> int
(** Compute the weight of a term, given a weight for variables
    and one for ID.ts.
    @param var unique weight for every variable (default 1)
    @param sym function from ID.ts to their weight (default [const 1])
    @since 0.5.3 *)

val ho_weight : t -> int

val ty_vars : t -> Type.VarSet.t
(** Set of free type variables *)

val is_ho_var : t -> bool

val is_ho_app : t -> bool
(** [is_ho_app (F t1…tn)] is true, when [F] is a variable (of any function type) *)

val as_ho_app : t -> (Type.t HVar.t * t list) option
(** [as_ho_app (F t1…tn) = Some (F, [t1…tn])] *)

val is_ho_pred : t -> bool
(** [is_ho_pred (F t1…tn)] is true, when [F] is a predicate variable *)

val is_ho_at_root : t -> bool
(** [is_ho_at_root t] returns [true] if the term [t] is a higher-order variable,
    possibly applied (i.e. [is_ho_var t || is_ho_app t]) *)

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
  ?filter_formula_subterms:(Builtin.t -> t list -> int list CCOpt.t) ->
  ?vars:bool ->
  ?ty_args:bool ->
  ?var_args:bool ->
  ?fun_bodies:bool ->
  ?pos:Position.t ->
  t ->
  t Position.With.t Iter.t
(** Iterate on all sub-terms with their position.
    @param vars specifies whether variables are folded on (default false).
    @param ty_args specifies whether type arguments are folded on (default true).
    @param var_args specifies whether arguments of applied variables are folded on (default true).
    @param fun_bodies specifies whether bodies of lambda-expressions are folded on (default true).
    @param pos the initial position (default empty) *)

(** {2 Some AC-utils} *)

module type AC_SPEC = sig
  val is_ac : ID.t -> bool
  val is_comm : ID.t -> bool
end

module AC (A : AC_SPEC) : sig
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

  val symbols : t Iter.t -> ID.Set.t
  (** Set of ID.ts occurring in the terms, that are AC *)

  val seq_symbols : t -> ID.t Iter.t
  (** Iter of AC symbols in this term *)
end

(** {2 Printing/parsing} *)

val print_all_types : bool ref
(** If true, {!pp} will print the types of all annotated terms *)

include Interfaces.PRINT with type t := t
include Interfaces.PRINT_DE_BRUIJN with type t := t and type term := t

val pp_var : Type.t HVar.t CCFormat.printer

val add_hook : print_hook -> unit
(** Hook used by default for printing *)

val default_hooks : unit -> print_hook list
(** List of default hooks *)

(* TODO
   include Interfaces.SERIALIZABLE with type t := t
*)

val debugf : Format.formatter -> t -> unit
(** debugf printing, with sorts *)

(** {2 Formulas} *)

module Form : sig
  val not_ : t -> t
  val equiv : t -> t -> t
  val xor : t -> t -> t
  val imply : t -> t -> t
  val eq : t -> t -> t
  val neq : t -> t -> t
  val and_ : t -> t -> t
  val or_ : t -> t -> t
  val and_l : t list -> t
  val or_l : t list -> t
  val forall : t -> t
  val exists : t -> t
  val choice : t -> t
end

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

val close_quantifier : Builtin.t -> Type.t list -> t -> t
val has_ho_subterm : t -> bool

(** {2 De Bruijn} *)
module DB : sig
  val is_closed : t -> bool
  val shift : ?depth:int -> int -> t -> t
  val unshift : ?depth:int -> int -> t -> t
  val eval : t DBEnv.t -> t -> t
  val unbound : t -> int list
  val skolemize_loosely_bound : ?already_sk:t IntMap.t -> t -> t * t IntMap.t
  val unskolemize : int Map.t -> Map.key -> t
  val map_vars_shift : ?depth:int -> int Map.t -> t -> t
end

(** {2 TPTP} *)

module TPTP : sig
  include Interfaces.PRINT with type t := t
  include Interfaces.PRINT_DE_BRUIJN with type t := t and type term := t and type print_hook := print_hook
end

module ZF : sig
  include Interfaces.PRINT with type t := t
end

val pp_in : Output_format.t -> t CCFormat.printer

module Conv : sig
  type ctx = Type.Conv.ctx

  val create : unit -> ctx
  val of_simple_term : ctx -> TypedSTerm.t -> t option

  val of_simple_term_exn : ctx -> TypedSTerm.t -> t
  (** @raise Type.Conv.Error on failure *)

  val to_simple_term : ?allow_free_db:bool -> ?env:TypedSTerm.t Var.t DBEnv.t -> ctx -> t -> TypedSTerm.t
  val var_to_simple_var : ?prefix:string -> ctx -> var -> TypedSTerm.t Var.t
end

(**/**)

val rebuild_rec : t -> t (* rebuild term fully, checking types *)

val fold_left_map2: ('acc1 -> 'acc2 -> 'a -> 'acc1 * 'acc2 * 'b) -> 'acc1 -> 'acc2 -> 'a list -> 'acc1 * 'acc2 * 'b list
val mangle_term : (Type.t * Type.t) list -> ((ID.t * Type.t) * t) list -> t -> (Type.t * Type.t) list * ((ID.t * Type.t) * t) list * t

(**/**)
