
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Type Inference}

    This module is used for two things that overlap:
    - inferring the types of symbols that have not been declared (e.g. in
    "fof" or "cnf" TPTP statements) so as to enrich a {!Signature}
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

type 'a or_error = ('a, string) CCResult.t

type type_ = TypedSTerm.t
type untyped = STerm.t (** untyped term *)
type typed = TypedSTerm.t (** typed term *)
type loc = ParseLocation.t

exception Error of string

val section : Util.Section.t

(** {2 Types for Builtins} *)

module TyBuiltin : sig
  val ty : Builtin.t -> type_ option
  val ty_exn : Builtin.t -> type_
end

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

  val create :
    ?def_as_rewrite:bool ->
    ?default:type_ ->
    ?on_var:[`Default | `Infer] ->
    ?on_undef:[`Warn | `Fail | `Guess] ->
    ?on_shadow:[`Warn | `Ignore] ->
    implicit_ty_args:bool ->
    unit ->
    t
  (** New context with a signature and default types.
      @param default which types are inferred by default (if not provided
        then {!type_erm} will be used)
      @param def_as_rewrite if true, definitions will be treated like rewrite rules
      @param on_undef behavior when an undefined identifier is met
      @param on_var behavior when a variable without type annotation is met
  *)

  val copy : t -> t
  (** Copy of the context *)

  val exit_scope : t -> unit
  (** Exit the current scope (formula, clause), meaning that all free
      variables' types are forgotten.
      Some free variables are bound to the [default] type provided
      at creation of the context.
      Some ree variables will be generalized, i.e., kept as (free) variables *)

  val declare : ?loc:loc -> t -> ID.t -> type_ -> unit
  (** Declare the type of a symbol, possibly shadowing a previous version  *)

  val pop_new_types : t -> (ID.t * type_) list
  (** Obtain the list of symbols whose type has been inferred recently,
      and reset it. *)
end

val unify : ?loc:loc -> type_ -> type_ -> unit

(** {2 Hindley-Milner Type Inference}

    This module, abstract in the exact kind of term it types, takes as input
    a signature and an {b untyped term}, and updates the typing context
    so that the {b untyped term} can be converted into a {b typed term}. *)

val infer_ty_exn : Ctx.t -> untyped -> type_
(** Type conversion from {!untyped}. *)

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

val infer_prop_exn : Ctx.t -> untyped -> typed
(** Same as {!infer_exn} but forces the type of its result
    to be {!TypedSTerm.prop} *)

val infer_clause_exn : Ctx.t -> untyped list -> typed list
(** Convert a clause. Free variables in each of the list's elements
    are shared *)

(** {3 Constraining types}

    This section is mostly useful for inferring a signature without
    converting untyped_terms into typed_terms. *)

val constrain_term_term_exn : ?loc:loc -> Ctx.t -> untyped -> untyped -> unit
(** Force the two terms to have the same type in this context
    @raise Error if an inconsistency is detected *)

val constrain_term_type_exn : ?loc:loc -> Ctx.t -> untyped -> type_ -> unit
(** Force the term's type and the given type to be the same.
    @raise Error if an inconsistency is detected *)

val constrain_term_term : ?loc:loc -> Ctx.t -> untyped -> untyped -> unit or_error
(** Safe version of {!constrain_term_term_exn} *)

val constrain_term_type : ?loc:loc -> Ctx.t -> untyped -> type_ -> unit or_error
(** Safe version of {!constrain_term_type_exn} *)

(** {2 Statements} *)

type typed_statement = (typed, typed, type_) Statement.t

val infer_statement_exn :
  ?file:string ->
  Ctx.t ->
  UntypedAST.statement ->
  typed_statement * typed_statement list
(** [infer_statement ctx ~f st] checks and convert [st] into a
    typed statements, and a list of auxiliary type declarations for symbols
    that were inferred implicitely. *)

val infer_statements_exn :
  ?def_as_rewrite:bool ->
  ?on_var:[`Infer | `Default] ->
  ?on_undef:[`Warn | `Fail | `Guess] ->
  ?on_shadow:[`Warn | `Ignore] ->
  ?ctx:Ctx.t ->
  ?file:string ->
  implicit_ty_args:bool ->
  UntypedAST.statement Sequence.t ->
  typed_statement CCVector.ro_vector
(** Infer all statements
    @param def_as_rewrite if true, definitions becomes rewrite rules *)

val infer_statements :
  ?def_as_rewrite:bool ->
  ?on_var:[`Infer | `Default] ->
  ?on_undef:[`Warn | `Fail | `Guess] ->
  ?on_shadow:[`Warn | `Ignore] ->
  ?ctx:Ctx.t ->
  ?file:string ->
  implicit_ty_args:bool ->
  UntypedAST.statement Sequence.t ->
  typed_statement CCVector.ro_vector or_error
