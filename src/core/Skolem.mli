
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Skolem symbols} *)

type type_ = TypedSTerm.t
type term = TypedSTerm.t
type form = TypedSTerm.t

type ctx
(** Context needed to create new symbols *)

val create :
  ?prefix:string -> ?prop_prefix:string ->
  ?on_new:(ID.t -> type_ -> unit) ->
  unit ->
  ctx
(** New skolem contex. A prefix can be provided, which will be
    added to all newly created skolem symbols.
    @param prefix used to name skolem functions/constants
    @param prop_prefix used to name sub-formulas during CNF
    @param on_id function called whenever a Skolem symbol is created *)

val fresh_sym : ctx:ctx -> ty:type_ -> ID.t
(** Just obtain a fresh skolem symbol. It is also declared
    in the inner signature. *)

val fresh_sym_with : ctx:ctx -> ty:type_ -> string -> ID.t
(** Fresh symbol with a different name *)

val pop_new_symbols : ctx:ctx -> (ID.t * type_) list
(** Remove and return the list of newly created symbols *)

(** {2 Skolemization} *)

val skolem_form : ctx:ctx -> (type_, term) Var.Subst.t -> type_ -> form -> term
(** [skolem_form ~ctx subst ty f] returns a term [t] made of a new symbol
    applied to the free variables of [f] that do not occur in [subst].
    This term should replace some free variable in [f] that has type [ty].

    For instance, [skolem_form ~ctx ø Y p(a, b, Y, X)] will yield
    something like [sk42(X)].
*)

(** {2 Definitions of Formulas} *)

type polarity =
  [ `Pos
  | `Neg
  | `Both
  ]

type definition = {
  form : form;
  proxy : form;
  polarity : polarity;
}

val define :
  ctx:ctx ->
  polarity:polarity ->
  form -> form
(** [rename_form ~ctx f] returns a new predicate for [f],
    with the free variables of [f] as arguments.

    @return the atomic formula that stands for [f]. *)

val pop_new_definitions : ctx:ctx -> definition list
(** List of new definitions, that were introduced since the last
    call to {!new_definitions}. The list can be obtained only once,
    after which those definitions are not "new" anymore.

    Will call {!remove_def} so there is no risk of re-using a definition
    with a new polarity. *)

