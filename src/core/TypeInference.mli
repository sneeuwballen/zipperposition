
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Type Inference}

    This module is used for two things that overlap:
    - inferring the types of symbols that have not been declared (e.g. in
    "fof" or "cnf" TPTP statements) so as to enrich a {!.signature}
    - converting {i untyped} terms or formulas into {i typed} formulas, by inferring
    the exact type of each subterm (and possibly inferring type parameters).

    In this context, {b generalizing} type variables means that if some ID.t
    whose type was unknown and its type still contains variables after the
    type inference, those variables are quantified instead of being bound to
    a default type (typically {!.Type.i}).

    For instance: say [f] is not declared and occurs in the term [f(f(nil))]
    with the declared constructor [nil : list(A)]. The inferred type for
    [f] should be something like [list(B) -> list(B)].
    - If we generalize, we declare that [f : list(A) -> list(A)] (for all [A]).
    - If we don't, we declare that [f : list($i) -> list($i)].

    Here we use a single scope when we unify and substitute type variables,
    the scope 0.

    Many functions will use an Error monad to make errors explicit. The error
    type is {!or_error}. The module {!CCError} in containers can be used
    to deal with errors (including monadic operators).
*)

type 'a or_error = [`Error of string | `Ok of 'a]

type type_ = TypedSTerm.t
type signature = type_ ID.Tbl.t
type untyped = STerm.t (** untyped term *)
type typed = TypedSTerm.t (** typed term *)

exception Error of string

(** {2 Typing context}

    This module provides a typing context, with an applicative interface.
    The context is used to map terms to types locally during type
    inference. It also keeps and updates a signature when symbols' types
    are inferred.

    This module is quite low-level, and shouldn't be used in simple cases
    (see the following modules)
*)

module Ctx : sig
  type t

  val create : ?default:type_ -> ?sigma:signature -> unit -> t
  (** New context with a signature and default types.
      @param sigma initial signature
      @param default which types are inferred by default (if not provided
        then {!type_erm} will be used) *)

  val copy : t -> t
  (** Copy of the context *)

  val exit_scope : t -> unit
  (** Exit the current scope (formula, clause), meaning that all free
      variables' types are forgotten. *)

  val declare : t -> ID.t -> type_ -> unit
  (** Declare the type of a ID.t. The type {b must} be equal to
      the current type of the ID.t, if any.
      @raise Type.Error if an inconsistency (with inferred types) is
        detected. *)

  val bind_to_default : t -> unit
  (** Free constructor variables are bound to the [default] type provided
      at creation of the context. *)

  val generalize : t -> unit
  (** Free constructor variables will be generalized, i.e., kept as (free) variables *)

  val to_signature : t -> signature
  (** Obtain the type of all symbols whose type has been inferred.
      If some instantiated variables remain, they are bound to the
      context's [default] parameter. *)
end

(** {2 Hindley-Milner Type Inference}

    This module, abstract in the exact kind of term it types, takes as input
    a signature and an {b untyped term}, and updates the typing context
    so that the {b untyped term} can be converted into a {b typed term}. *)

val infer_ty : Ctx.t -> untyped -> type_ or_error
(** Type conversion from {!untyped}. *)

val infer_exn : Ctx.t -> untyped -> typed
(** Infer the type of this term under the given signature. This updates
    the context's typing environment!

    @param ctx the context
    @param untyped the untyped term whose type must be inferred

    @raise Error if the types are inconsistent *)

val infer : Ctx.t -> untyped -> typed or_error
(** Safe version of {!infer_exn}. It returns [`Error s] rather
    than raising {!Error} if the typechecking fails. *)

(** {3 Constraining types}

    This section is mostly useful for inferring a signature without
    converting untyped_terms into typed_terms. *)

val constrain_term_term_exn : Ctx.t -> untyped -> untyped -> unit
(** Force the two terms to have the same type in this context
    @raise Error if an inconsistency is detected *)

val constrain_term_type_exn : Ctx.t -> untyped -> type_ -> unit
(** Force the term's type and the given type to be the same.
    @raise Error if an inconsistency is detected *)

val constrain_term_term : Ctx.t -> untyped -> untyped -> unit or_error
(** Safe version of {!constrain_term_term_exn} *)

val constrain_term_type : Ctx.t -> untyped -> type_ -> unit or_error
(** Safe version of {!constrain_term_type_exn} *)
