
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Substitutions} *)

(** Substitutions map (scoped) variables to terms/types.

    They work on free variables (within a scope, so that the same variable can
    live within several scopes).

    The concept of scope is to allow the same free variable to be used in
    several contexts without being renamed. A scope is kind of a namespace,
    where variables from distinct namespaces are always distinct.
*)

type term = InnerTerm.t
type var = InnerTerm.t HVar.t

(** {2 Renamings}
    A renaming is used to disambiguate variables that come from distinct
    scopes but have the same index. It is used to merge together several scopes, in
    a sound way, by ensuring variables from those scopes are mapped to distinct
    variables of the new scope. For instance, a given renaming applied to (X,0) and
    (X,1) will return two different variables, as if one of the X had been renamed
    prior to unification/binding. *)

module Renaming : sig
  type t

  val create : unit -> t
  (** Make a fresh renaming. It can only grow, so it is safe to use
      it as a value. *)

  val none: t
  (** Renaming that doesn't actually rename(!) *)
end

(** {3 Basics} *)

type t
(** A substitution that binds term variables to other terms *)

type subst = t

val empty : t
(** The identity substitution *)

val is_empty : t -> bool
(** Is the substitution empty? *)

(** {3 Operations on Substitutions} *)

val find_exn : t -> var Scoped.t -> term Scoped.t
(** Lookup variable in substitution.
    @raise Not_found if variable not bound. *)

val find : t -> var Scoped.t -> term Scoped.t option

val deref : t -> term Scoped.t -> term Scoped.t
(** [deref t s_t] dereferences [t] as long as [t] is a variable bound
    in [subst] *)

val get_var : t -> var Scoped.t -> term Scoped.t option
(** Lookup recursively the var in the substitution, until it is not a
    variable anymore, or it is not bound.
    @return None if the variable is not bound, [Some (deref (t, sc_t))]
      if [v] is bound to [t, sc_t] *)

val mem : t -> var Scoped.t -> bool
(** Check whether the variable is bound by the substitution *)

exception InconsistentBinding of var Scoped.t * term Scoped.t * term Scoped.t

val bind : t -> var Scoped.t -> term Scoped.t -> t
(** Add [v] -> [t] to the substitution. Both terms have a context.
    It is {b important} that the bound term is De-Bruijn-closed (assert).
    @raise InconsistentBinding if [v] is already bound in
      the same context, to another term. *)

val update : t -> var Scoped.t -> term Scoped.t -> t
(** Replace [v] -> ? by  [v] -> [t] in the substitution. Both terms have a context.
    It is {b important} that the bound term is De-Bruijn-closed (assert).
    @raise InconsistentBinding if [v] is not yet bound in the same context. *)

val merge : t -> t -> t
(** [merge s1 s2] is the substitution that maps [t] to [(s1 t)] or to [(s2 t)].
    @raise InconsistentBinding if the substitutions disagree. *)

val remove : t -> var Scoped.t -> t
(** Remove the given binding. No other variable should depend on it... *)

val filter_scope : t -> Scoped.scope -> t
(** Only keep bindings from this scope *)

(** {2 Set operations} *)

val domain : t -> (var Scoped.t) Sequence.t
(** Domain of substitution *)

val codomain : t -> (term Scoped.t) Sequence.t
(** Codomain (image terms) of substitution *)

val introduced : t -> (var Scoped.t) Sequence.t
(** Variables introduced by the substitution (ie vars of codomain) *)

val normalize : t -> t
(** Normalize bindings that are in the same scope.
    E.g. [x0 -> f(y0), y0 -> g(z0), z0->a]
    becomes [x0->f(g(a))0, y0->g(a)0, z0->g(z0)] *)

val map : (term -> term) -> t -> t
(** Map on term *)

val filter : (var Scoped.t -> term Scoped.t -> bool) -> t -> t
(** Filter bindings *)

(*
val compose : t -> t -> t
  (** [compose s1 s2] is the substitution that to [x] associates
      [s1 (s2 x)]. *)
*)

val is_renaming : t -> bool
(** Check whether the substitution is a variable renaming *)

val equal : t -> t -> bool

val compare : t -> t -> int

val hash : t -> int

include Interfaces.PRINT with type t := t

val pp_bindings : t CCFormat.printer
(** Only print the bindings, no box *)

val fold : ('a -> var Scoped.t -> term Scoped.t -> 'a) -> 'a -> t -> 'a
val iter : (var Scoped.t -> term Scoped.t -> unit) -> t -> unit

val to_seq : t -> (var Scoped.t * term Scoped.t) Sequence.t
val to_list : t -> (var Scoped.t * term Scoped.t) list
val of_seq : ?init:t -> (var Scoped.t * term Scoped.t) Sequence.t -> t
val of_list : ?init:t -> (var Scoped.t * term Scoped.t) list -> t

(** {2 Applying a substitution} *)

val apply : Renaming.t -> t -> term Scoped.t -> term
(** Apply the substitution to the given term.
    This function assumes that all terms in the substitution are closed,
    and it will not perform De Bruijn indices shifting. For instance,
    applying [{X -> f(db0)}] (with [db0] the De Bruijn index [0])
    to the term [forall. p(X)] will yield [forall. p(f(db0))] (capturing)
    and not [forall. p(f(db1))].
    @param renaming used to desambiguate free variables from distinct scopes *)

(** {2 Specializations} *)

module type SPECIALIZED = sig
  type term
  type t = subst

  val find_exn : t -> var Scoped.t -> term Scoped.t

  val get_var : t -> var Scoped.t -> term Scoped.t option

  val deref : t -> term Scoped.t -> term Scoped.t

  val apply : Renaming.t -> t -> term Scoped.t -> term
  (** Apply the substitution to the given term/type.
      @param renaming used to desambiguate free variables from distinct scopes *)

  val bind : t -> var Scoped.t -> term Scoped.t -> t
  (** Add [v] -> [t] to the substitution. Both terms have a context.
      @raise InconsistentBinding if [v] is already bound in
        the same context, to another term. *)

  val update : t -> var Scoped.t -> term Scoped.t -> t
  (** Replace [v] -> ? by  [v] -> [t] in the substitution. Both terms have a context.
      @raise InconsistentBinding if [v] is not yet bound in the same context. *)

  val of_list : ?init:t -> (var Scoped.t * term Scoped.t) list -> t
end

module Ty : SPECIALIZED with type term = Type.t

module FO : sig
  include SPECIALIZED with type term = Term.t
  val bind' : t -> Type.t HVar.t Scoped.t -> term Scoped.t -> t
  val apply_l : Renaming.t -> t -> term list Scoped.t -> term list
  val of_list' : ?init:t -> (Type.t HVar.t Scoped.t * term Scoped.t) list -> t
  val map : (term -> term) -> t -> t
  val filter : (Type.t HVar.t Scoped.t -> term Scoped.t -> bool) -> t -> t
end

(** {2 Projections for proofs} *)

module Projection : sig
  type t = private {
    scope: Scoped.scope;
    subst: subst;
    renaming: Renaming.t;
  }
  (** A representation of the substitution for a given scope, after applying
      the renaming. *)

  val subst : t -> subst
  val scope : t -> Scoped.scope
  val renaming : t -> Renaming.t

  val bindings : t -> (var * term) list
  (** List of bindings of the projection.
      Variables in the domain are bound in [scope subst], but variables
      in terms of the codomain are bound in the renaming *)

  val as_inst :
    ?allow_free_db:bool ->
    ctx:Term.Conv.ctx ->
    t ->
    Type.t HVar.t list ->
    (TypedSTerm.t,TypedSTerm.t) Var.Subst.t
  (** Convert into an instantiation on the given variables *)

  val is_empty : t -> bool

  val make : Renaming.t -> subst Scoped.t -> t

  val pp : t CCFormat.printer
end
