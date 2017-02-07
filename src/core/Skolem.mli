
(* This file is free software, part of Libzipperposition. See file "license" for more details. *)

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

val fresh_skolem : ctx:ctx -> ty:type_ -> ID.t
(** Just obtain a fresh skolem symbol. It is also declared
    in the inner signature. *)

val fresh_skolem_prefix : ctx:ctx -> ty:type_ -> string -> ID.t
(** Fresh symbol with a different name *)

val pop_new_skolem_symbols : ctx:ctx -> (ID.t * type_) list
(** Remove and return the list of newly created Skolem symbols *)

(** {2 Skolemization} *)

val skolem_form : ctx:ctx -> (type_, term) Var.Subst.t -> type_ Var.t -> form -> term
(** [skolem_form ~ctx subst var f] returns a term [t] made of a new symbol
    applied to the free variables of [f] that do not occur in [subst].
    This term should replace the variable [var], occurring free in [f].

    For instance, [skolem_form ~ctx ø Y p(a, b, Y, X)] will yield
    something like [sk42(X)].
*)

(** {2 Definitions of Formulas} *)

type polarity =
  [ `Pos
  | `Neg
  | `Both
  ]

val pp_polarity : polarity CCFormat.printer

type form_definition = private {
  form: form;
  (* the defined object *)
  proxy : term;
  (* atom/term standing for the defined object *)
  add_rules: bool;
  (* do we add the add rules
     [proxy -> true if form]
     [proxy -> false if not form] (depending on polarity) *)
  polarity : polarity;
}

val pp_form_definition : form_definition CCFormat.printer

val define_form :
  ctx:ctx ->
  add_rules:bool ->
  polarity:polarity ->
  form ->
  form_definition
(** [define ~ctx f] returns a new predicate for [f],
    with the free variables of [f] as arguments.

    @return the atomic formula that stands for [f]. *)

type term_definition = private {
  td_id: ID.t;
  td_ty: type_;
  td_rules: (form, term, type_) Statement.def_rule list;
}

val define_term :
  ctx:ctx ->
  (term list * term) list ->
  term_definition
(** [define_term l] introduces a new function symbol [f] that is
    defined by:
    - for each [args, rhs] in [l], [f args = rhs] *)

type definition =
  | Def_form of form_definition
  | Def_term of term_definition

val pp_definition : definition CCFormat.printer

(* TODO: return list of sum type [form_def | term_def] *)
val pop_new_definitions : ctx:ctx -> definition list
(** List of new definitions, that were introduced since the last
    call to {!new_definitions}. The list can be obtained only once,
    after which those definitions are not "new" anymore.

    Will call {!remove_def} so there is no risk of re-using a definition
    with a new polarity. *)

(** {2 Attribute} *)

type kind = K_normal | K_ind (* inductive *)

exception Attr_skolem of kind

val is_skolem : ID.t -> bool
(** [is_skolem id] returns [true] iff [id] is a Skolem symbol *)


