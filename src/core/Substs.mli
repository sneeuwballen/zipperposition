
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Substitutions}

    Substitutions map variables to terms/types. They work on free variables (within
    a scope, so that the same variable can live within several scopes).

    The concept of scope is to allow the same free variable to be used in
    several contexts without being renamed. A scope is kind of a namespace,
    where variables from distinct namespaces are always distinct.
*)

type scope = int
(** A scope is an integer. Variables can only be bound in one scope,
    and variables from distinct scopes are distinct too. *)

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
  val clear : t -> unit
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

exception KindError

val bind : t -> var Scoped.t -> term Scoped.t -> t
(** Add [v] -> [t] to the substitution. Both terms have a context.
    It is {b important} that the bound term is De-Bruijn-closed (assert).
    @raise Invalid_argument if [v] is already bound in
      the same context, to another term.
    @raise KindError if a term of a given kind is bound to a term
      of another kind (see {!ScopedTerm.Kind.t}) *)

val append : t -> t -> t
(** [append s1 s2] is the substitution that maps [t] to [s2 (s1 t)]. *)

val remove : t -> var Scoped.t -> t
(** Remove the given binding. No other variable should depend on it... *)

(** {2 Set operations} *)

val domain : t -> (var Scoped.t) Sequence.t
(** Domain of substitution *)

val codomain : t -> (term Scoped.t) Sequence.t
(** Codomain (image terms) of substitution *)

val introduced : t -> (var Scoped.t) Sequence.t
(** Variables introduced by the substitution (ie vars of codomain) *)

(*
val compose : t -> t -> t
  (** [compose s1 s2] is the substitution that to [x] associates
      [s1 (s2 x)]. *)
*)

val is_renaming : t -> bool
(** Check whether the substitution is a variable renaming *)

include Interfaces.PRINT with type t := t

val fold : ('a -> var Scoped.t -> term Scoped.t -> 'a) -> 'a -> t -> 'a
val iter : (var Scoped.t -> term Scoped.t -> unit) -> t -> unit

val to_seq : t -> (var Scoped.t * term Scoped.t) Sequence.t
val to_list : t -> (var Scoped.t * term Scoped.t) list
val of_seq : ?init:t -> (var Scoped.t * term Scoped.t) Sequence.t -> t
val of_list : ?init:t -> (var Scoped.t * term Scoped.t) list -> t

(** {2 Applying a substitution} *)

val apply : t -> renaming:Renaming.t -> term Scoped.t -> term
(** Apply the substitution to the given term.
    This function assumes that all terms in the substitution are closed,
    and it will not perform De Bruijn indices shifting. For instance,
    applying [{X -> f(db0)}] (with [db0] the De Bruijn index [0])
    to the term [forall. p(X)] will yield [forall. p(f(db0))] (capturing)
    and not [forall. p(f(db1))].
    @param renaming used to desambiguate free variables from distinct scopes *)

val apply_no_renaming : t -> term Scoped.t -> term
(** Same as {!apply}, but performs no renaming of free variables.
    {b Caution}, can entail collisions between scopes! *)

(** {2 Specializations} *)

module type SPECIALIZED = sig
  type term
  type t = subst

  val apply : t -> renaming:Renaming.t -> term Scoped.t -> term
  (** Apply the substitution to the given term/type.
      @param renaming used to desambiguate free variables from distinct scopes *)

  val apply_no_renaming : t -> term Scoped.t -> term
  (** Same as {!apply}, but performs no renaming of free variables.
      {b Caution}, can entail collisions between scopes! *)

  val bind : t -> var Scoped.t -> term Scoped.t -> t
  (** Add [v] -> [t] to the substitution. Both terms have a context.
      @raise Invalid_argument if [v] is already bound in
        the same context, to another term. *)
end

module Ty : SPECIALIZED with type term = Type.t

module FO : SPECIALIZED with type term = FOTerm.t

module HO : SPECIALIZED with type term = HOTerm.t
